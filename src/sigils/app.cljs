(ns sigils.app
  (:require [clojure.string :as str]
            [om.core :as om]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom]))

(enable-console-print!)

;; randomness stuff

(defn rand-between [lower upper]
  (+ lower (* (rand) (- upper lower))))

;; geometry stuff

(defn deg->rad [deg]
  (* deg (/ js/Math.PI 180)))

(defn rad->deg [rad]
  (* rad (/ 180 js/Math.PI)))

(defn sin [deg]
  (js/Math.sin (deg->rad deg)))

(defn cos [deg]
  (js/Math.cos (deg->rad deg)))

(defn square [n]
  (* n n))

(defn displace [point angle distance]
  {:x (+ (:x point) (* distance (sin angle)))
   :y (+ (:y point) (* distance (cos angle)))})

(defn distance [{x1 :x y1 :y} {x2 :x y2 :y}]
  (js/Math.sqrt (+ (square (- x2 x1))
                   (square (- y2 y1)))))

(defn midpoint [{x1 :x y1 :y} {x2 :x y2 :y}]
  {:x (/ (+ x1 x2) 2)
   :y (/ (+ y1 y2) 2)})

(defn point-at-angle [circle angle]
  (displace (:center circle) angle (:radius circle)))

(defn point->str [point]
  (str (:x point) "," (:y point)))

(defn rotations [angle n]
  (let [base-angle (rand-nth [0 0 90 180 180 270])
        offsets (range base-angle (+ base-angle 360) (/ 360 n))]
    (mapv #(+ angle %) offsets)))

;; shape generation stuff

(declare circle-with-augmentations)

(defn gen-concentric-circle [circle]
  (circle-with-augmentations
    {:type :circle
     :center (:center circle)
     :radius (rand-between (* (:radius circle) (/ 1 4)) (* (:radius circle) (/ 3 4)))
     :base-angle (rand-int 360)}))

(defn gen-chord [outer-circle inner-circle]
  (let [outer-r (:radius outer-circle)
        midpoint-dist-from-center (- outer-r (* 2 (:radius inner-circle)))
        midpoint-angle-from-center (:base-angle inner-circle)
        adj midpoint-dist-from-center
        hyp outer-r
        angle-offset (rad->deg (js/Math.acos (/ adj hyp)))
        intersect-angle-a (+ midpoint-angle-from-center angle-offset)
        intersect-angle-b (- midpoint-angle-from-center angle-offset)]
    {:type :line
     :point-a (displace (:center outer-circle) intersect-angle-a outer-r)
     :point-b (displace (:center outer-circle) intersect-angle-b outer-r)}))

(defn gen-inscribed-circle [circle]
  (let [{outer-center :center, outer-r :radius} circle
        angle (rand-int 360)
        inner-r (rand-between (* outer-r (/ 1 4)) (* outer-r (/ 3 4)))
        inner-center (displace outer-center angle (- outer-r inner-r))
        inner-circle {:type :circle
                      :center inner-center
                      :radius inner-r
                      :base-angle angle}]
    (cond-> (circle-with-augmentations inner-circle)
            (> (rand) 0.5)
            (conj (gen-chord circle inner-circle)))))

(defn gen-inscribed-circle-for-polygon [polygon]
  (let [[point-a point-b] (take 2 (:points polygon))
        mid (midpoint point-a point-b)
        radius (distance (:center polygon) mid)]
    (circle-with-augmentations
      {:type :circle
       :center (:center polygon)
       :radius radius
       :base-angle (:base-angle polygon)})))

(defn gen-inscribed-polygon [circle]
  (let [base-angle (:base-angle circle)
        n-points (rand-nth [3 3 3 4 4 4 5 6 6])
        point-angles (rotations base-angle n-points)
        polygon {:type :polygon
                 :points (mapv (partial point-at-angle circle) point-angles)
                 :center (:center circle)
                 :base-angle base-angle}]
    (cond-> [polygon]
            (> (rand) 0.5)
            (into (gen-inscribed-circle-for-polygon polygon)))))

(defn gen-line-thru-center-for-circle [circle]
  [{:type :line
    :point-a (point-at-angle circle (:base-angle circle))
    :point-b (point-at-angle circle (+ (:base-angle circle) 180))}])

(defn circle-with-augmentations [circle]
  (let [gen-f (rand-nth [gen-concentric-circle
                         gen-inscribed-circle
                         gen-inscribed-polygon
                         gen-line-thru-center-for-circle
                         (constantly [])])]
    (into [circle] (gen-f circle))))

(defn gen-box []
  (let [circle {:type :circle
                :center {:x 0 :y 0}
                :radius 75
                :base-angle (rand-nth [0 0 90 180 180 270])}
        gen-f (rand-nth [gen-concentric-circle
                         gen-inscribed-circle
                         gen-inscribed-polygon])]
    (into [circle] (gen-f circle))))

;; rendering stuff

(defonce app-state
  (atom {:boxes (vec (repeatedly 21 gen-box))}))

(defcomponent graphic-view [data owner]
  (render [_]
    (case (:type data)
      :circle
        (dom/circle {:cx (:x (:center data))
                     :cy (:y (:center data))
                     :r (:radius data)
                     :fill "none"
                     :stroke "green"
                     :stroke-width 1})
      :polygon
        (dom/polygon {:points (str/join " " (map point->str (:points data)))
                      :fill "none"
                      :stroke "green"
                      :stroke-width 1})
      :line
        (dom/line {:x1 (:x (:point-a data))
                   :y1 (:y (:point-a data))
                   :x2 (:x (:point-b data))
                   :y2 (:y (:point-b data))
                   :stroke "green"
                   :stroke-width 1}))))

(defcomponent box-view [data owner]
  (render [_]
    (prn data)
    (dom/svg {:viewBox "0 0 200 200"
              :preserveAspectRatio "xMidYMid meet"
              :width 200 :height 200}
      (dom/g {:transform "translate(100 100)"}
        (om/build-all graphic-view data)))))

(defcomponent app [data owner]
  (render [_]
    (dom/div {:class "app"}
      (om/build-all box-view (:boxes data)))))

(om/root app app-state {:target (js/document.getElementById "app")})
