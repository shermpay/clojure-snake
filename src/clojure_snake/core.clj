;;; Sherman Pay Jing Hao
;;; Saturday, 01. March 2014
;;; Simple snake game

(ns clojure-snake.core
  (:gen-class)
  (:require [seesaw.core :as seesaw]
            [seesaw.graphics :as graphics]
            [seesaw.color :as color])
  (:import [java.awt.event KeyEvent]))

(def width 80)
(def height 60)
(def screen-width (* width 10))
(def screen-height (* height 10))
(def point-size 10)
(def turn-millis 75)
(def win-length 10)
(def dirs {KeyEvent/VK_LEFT [-1 0]
           KeyEvent/VK_RIGHT [1 0]
           KeyEvent/VK_UP   [0 -1]
           KeyEvent/VK_DOWN [0 1]})

(defn add-points 
  "Add all the points together [10 10] + [-1 0] = [9 10]. Returns a
  vector"
  [& args]
  (vec (apply map + args)))

(defn point-to-screen-rect 
  "point to screen rect [x y] -> [x * point-size, y * point-size, point-size, point-size]"
  [pt]
  (map #(* point-size %)
       [(pt 0) (pt 1) 1 1]))

(defn create-apple 
  "Returns a map of an apple with a random location"
  []
  {:location [(rand-int width) (rand-int height)]
   :color (color/color "orange")
   :type :apple})

(defn create-snake 
  "Returns a map of a snake with a initial size and direction"
  []
  {:body (list [1 1])
   :dir [0 1]                           ;Default direction
   :color (color/color "green")
   :type :snake})

(defn move 
  "Moves the snake in a specific dir. If grow is true add 1 rect to
  the back of the snake"
  [{:keys [body dir] :as snake} & grow]
  (assoc snake :body (cons (add-points (first body) dir)
                           (if grow body (butlast body)))))

(defn win? 
  "Wins when snake has reached certain length"
  [{body :body}]
  (= win-length (count body)))

(defn lose?
  "Wins when snake has reached certain length"
  [{[head & body] :body}]
  (or (contains? (set body) head)
      (>= (first head) width)
      (< (first head) 0)
      (> (second head) height)
      (< (second head) 0)))

(defn eats?
  "Returns true if head of snake and apple are in the same location"
  [{[snake-head] :body} {apple :location}]
  (= snake-head apple))

(defn turn 
  "Turns the snake in newdir"
  [snake newdir]
  (if (= [0 0] (add-points newdir (:dir snake)))
    snake
    (assoc snake :dir newdir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; MUTABLE ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reset-game
  "Resets the game. Move the snake back to it's initial position and
  apple to it's initial position"
  [snake apple score]
  (dosync (ref-set apple (create-apple))
          (ref-set snake (create-snake))
          (ref-set score 0))
  nil)

(defn update-direction
  "Updates the direction of the snake"
  [snake newdir]
  (when newdir
    (dosync (alter snake turn newdir))))

(defn update-positions
  "Updates the positions of snake and apple"
  [snake apple score]
  (dosync
   (if (eats? @snake @apple)
     (do
       (ref-set apple (create-apple))
       (alter snake move :grow)
       (alter score + 1))
     (alter snake move))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; GUI ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fill-point
  "Fills in a single point given graphics g and color"
  [g pt color]
  (let [[x y width height] (point-to-screen-rect pt)]
    (graphics/draw g (graphics/rect x y width height)
                   (graphics/style :background color))))

(defmulti paint "renders objects" (fn [g object & _] (:type object)))

(defmethod paint :apple [g {:keys [location color]}]
  (fill-point g location color))

(defmethod paint :snake [g {:keys [body color]}]
  (doseq [point body]
    (fill-point g point color)))

(defn game-canvas
  "Game canvas. Takes a snake and apple to render."
  [snake apple]
  (seesaw/canvas :id :game-canvas
                 :paint (fn [c g]
                          (try
                            (doto g
                              (graphics/anti-alias)
                              (paint @apple)
                              (paint @snake)) 
                            (catch Exception e
                              (println e))))))

(defn game
  "Game function."
  []
  (let [snake (ref (create-snake))
        apple (ref (create-apple))
        score (ref 0)
        canvas (game-canvas snake apple)
        score-label (seesaw/label :id :score-label
                                  :text (str "Score: " @score)
                                  :background (color/color "blue")
                                  :foreground (color/color "white"))
        game-panel (seesaw/border-panel :center canvas
                                        :south score-label)
        game-frame (seesaw/frame :id :game-frame
                                 :title "Snake!"
                                 :width screen-width
                                 :height (+ screen-height 50)
                                 :visible? true
                                 :content game-panel
                                 :on-close :exit)
        timer (seesaw/timer (fn [e]
                              (do
                                (update-positions snake apple score)
                                (when (lose? @snake)
                                  (seesaw/alert "You lose...")
                                  (reset-game snake apple score))
                                (seesaw/repaint! game-panel)
                                (seesaw/config! score-label :text (str "Score: " @score))))
                            :start? true
                            :delay 30)]
    (seesaw.core/listen game-frame :key-pressed
                        (fn [k] 
                          (update-direction snake (dirs (.getKeyCode k)))))))


(defn -main
  "Game"
  [& args]
  (game))
