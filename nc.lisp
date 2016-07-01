(eval-when (:compile-toplevel) (ql:quickload :cgen))
(in-package :cg-user)
(switch-reader)

(defvar *boiler-func* nil)

(defmacro %function (name parameters -> type &body body)
  "This is a macro for defining the general helper functions. Push
name, forward declaration and function code into *boiler-func*"
  `(cl:push '((:name ,name)
	      (:fwd  (function ,name ,parameters ,-> ,type))
	      (:code (progn
		       (function ,name ,parameters -> ,type
				 ,@body))))
	    *boiler-func*))

(defun get-builtin-fwd (alist)
  (second (assoc :fwd alist)))
(defun get-builtin-name (alist)
  (second (assoc :name alist)))
(defun get-builtin-code (alist)
  (second (assoc :code alist)))

(defmacro gen-builtin-forward-declaration ()
  "Generate forward declarations for all the functions in the C file."
  `(progn
     ,@(loop for e in *boiler-func* collect
	  `(progn ,(get-builtin-fwd e)
		  (comment ";" :prefix "")))))

(defmacro gen-builtin-code ()
  "Emit the code for all the functions."
  `(progn
     ,@(loop for e in
	    *boiler-func* collect
	  (get-builtin-code e))))

(defmacro inc (x)
  `(set ,x (+ 1 ,x)))
(defmacro dec (x)
  `(set ,x (- ,x 1)))

(defmacro when (clause &body body)
  `(if ,clause
       (progn
	 ,@body)))

(use-variables NULL
	       bool
	       TRUE
	       FALSE
	       ERR
	       OK
	       COLOR_RED
	       COLOR_GREEN
	       COLOR_YELLOW
	       COLOR_BLUE
	       COLOR_CYAN
	       COLOR_MAGENTA
	       COLOR_WHITE
	       COLOR_BLACK
	       stdscr)


(progn
    (load "boiler"))

#+nil
(let ()
  (with-open-file (*standard-output* "nc.c"
				     :direction :output
				     :if-exists :supersede
				     :if-does-not-exist :create)
    (loop for e in (list
		    (comment "Headers")
		    (include <ncurses.h>)

		    (comment "forward declarations")
		    (gen-builtin-forward-declaration)
		    (comment "Global variables")
		    
		    (gen-builtin-code)
		    
		    (function main ((int argc) (char** argv)) -> int
		      (comment "(void) argc;" :prefix "")
		      (comment "(void) argv;" :prefix "")
		      (funcall init)
		      (return 0))) 
       do
	 (simple-print e))))
