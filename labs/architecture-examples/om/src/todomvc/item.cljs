(ns todomvc.item
  (:require [cljs.core.async :refer [>! put!]]
            [todomvc.utils :refer [now hidden]]
            [clojure.string :as string]
            [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]]))

(def ESCAPE_KEY 27)
(def ENTER_KEY 13)

;; =============================================================================
;; Todo Item

;; -----------------------------------------------------------------------------
;; Event Handlers

(defn submit [e todo owner comm]
  (when-let [edit-text (om/get-state owner :edit-text)]
    (if-not (string/blank? (.trim edit-text))
      (do
        (om/update! todo :title edit-text)
        (put! comm [:save @todo]))
      (put! comm [:destroy @todo])))
  false)

(defn edit [e todo owner comm]
  (let [todo @todo
        node (om/get-node owner "editField")]
    (put! comm [:edit todo])
    (doto owner
      (om/set-state! :needs-focus true)
      (om/set-state! :edit-text (:title todo)))))

(defn key-down [e todo owner comm]
  (condp == (.-keyCode e)
    ESCAPE_KEY (let [todo @todo]
                 (om/set-state! owner :edit-text (:title todo))
                 (put! comm [:cancel todo]))
    ENTER_KEY  (submit e todo owner comm)
    nil))

(defn change [e todo owner]
  (om/set-state! owner :edit-text (.. e -target -value)))

;; -----------------------------------------------------------------------------
;; Todo Item
(defn todo-item [todo owner]
  (reify
    om/IInitState
    (init-state [_]
      {:edit-text (:title todo)})
    om/IDidUpdate
    (did-update [_ _ _]
      (when (and (:editing todo)
                 (om/get-state owner :needs-focus))
        (let [node (om/get-node owner "editField")
              len  (.. node -value -length)]
          (.focus node)
          (.setSelectionRange node len len))
        (om/set-state! owner :needs-focus nil)))
    om/IRenderState
    (render-state [_ {:keys [comm] :as state}]
      (let [class (cond-> ""
                    (:completed todo) (str "completed ")
                    (:editing todo)   (str "editing"))]
        (html
         [:li.class {:style (hidden (:hidden todo))}
          [:div.view
           [:input.toggle
            {:type "checkbox"
             :checked (and (:completed todo) "checked")
             :onChange (fn [_]
                         (println "onChange is called")
                         (om/transact! todo :completed #(not %)))}]
           [:label {:onDoubleClick #(edit % todo owner comm)}
            (:title todo)]
           [:button.destroy {:onClick (fn [_] (put! comm [:destroy @todo]))}]]
          [:input.edit
           {:ref "editField"
            :value (om/get-state owner :edit-text)
            :onBlur #(submit % todo owner comm)
            :onChange #(change % todo owner)
            :onKeyDown #(key-down % todo owner comm)}]])))))
