
;;This function is called to delete a student from the roster, either by name or ID. Depending on 
;;which of the two option is entered, the function then starts at the head of the roster and is 
;;recursively called upon minus the head until a match is found. 

(define stud-delete
	(lambda (input lst)
		(cond  ((null? lst)
				(display "\n\tStudent ")
				(display input)
				(display " is not in the roster.\n")
			) 
			((and (string? input) (equal? (cadr (car lst)) input))
				(display "\n\t Student with name ")
				(display input)
				(display " deleted. \n")
				(cdr lst)
			)
			((and (equal? (car (car lst)) input))
				(display "\n\t Student with ID ")
                                (display input)
                                (display " deleted. \n")
				(cdr lst)
			)
			(else
				(cons (car lst) (stud-delete input (cdr lst)))
			)
		)
	)
)	

;;This function is called to display information pertaining to a specific student. Since the 
;;output for each input case (ID or Name) is the same, I was able to combine them into the same 
;;condition using the 'or' special form, unlike the previous funtion where the output differs.  

(define stud-info
	(lambda (input lst)
		(cond 
			((null? lst)
				(display "\n\tStudent ")
				(display input)
				(display " is not in the roster.\n")
			) 
			((or (equal? (cadr (car lst)) input) (equal? (car (car lst)) input))
				(display "\n\tID=")
				(display (caar lst))
				(display ", Name=")
				(display (cadr (car lst)))
				(display ", Grade=")
				(display (caddr (car lst)))
				(display "\n")
			)
			(else
				(stud-info input (cdr lst))
			)
		)
	)
)

;;This function is used as a parameter to the view-roster function in order to sort the roster by 
;;student ID. Im not sure if this is the correct use of casting since the ID's are numbers, but 
;;when I tried replacing (string<?) with (number<?) I was not obtaining the correct output.

(define by-id
        (lambda (first second)
                (cond ((string<? (car first) (car second)) #t)
                (else #f)
                )
        )
)

;;This function is used as a parameter to the view-roster function in order to sort the roster by 
;;student name. Here, it made perfect sense to me to use casting making each object (first & 
;;second) string types unlike the by-id function. 

(define by-name
	(lambda (first second)
		(cond ((string<? (cadr first) (cadr second)) #t)
		(else #f)
		)
	)
)

;;This function is used as a parameter to the view-roster function in order to sort the roster by 
;;grade in ascending order. No casting was needed here because the condition could be checked with 
;;basic numeric comparison... However I am still unsure why it would not allow me to do this with 
;;by-id function.

(define by-grade
	(lambda (first second)
		(cond ((< (caddr first) (caddr second)) #t)
		(else #f)
		)
	)
)

;;This function is used to display the roster in a specified manner (either sorted by ID, name, or 
;;grade) which is passed to the function when called. If the roster is empty, display error 
;;message.

(define view-roster
        (lambda(lst x)
                (cond ((null? lst))
                       (else
                                (display "\n\t No.")
                                (display x)
                                (display ": ID=")
                                (display (car(car lst)))
                                (display ", Name=")
                                (display (cadr(car lst)))
                                (display ", Grade=")
                                (display (car(cdr(cdr(car lst)))))
                                (view-roster (cdr lst) (+ x 1))
                        )
                )
        )
)

;;This function allows a user to add a student's information to the roster, including: ID, name, 
;;and grade. I found this function fairly easy to write becase of the example t2.scm. However, it 
;;took me a while to realize that with this function the implementation shown in t1.scm was not 
;;needed and was causing errors in other parts of my program until I deleted it.

(define sec-input-stud
	(lambda(n lst)
		(cond ((= n 0) (begin
				(display "\t Student ID: ")
				(sec-input-stud 1 (list (read-line)))
				))
		      ((= n 1) (begin
				(display "\n\t Student name: ")
				(sec-input-stud 2 (list (car lst) (read-line)))
				))
		      ((= n 2) (begin
				(display "\n\t Grade: ")
				(list (car lst) (cadr lst) (read))
				))
		)
	) 
)

;;This function controls the action performed by each menu choice. I had trouble at first with the 
;;load/store file functionality. Originally I did not realize the need for the use of 'let' 
;;special form so that I could use 'infile' and 'outfile' as local bindings to the name entered by 
;;a user. 

(define performtask
	(lambda (n roster)
		(cond((= n 0) (begin
				(display "\n\t Roster reset (now empty).")
				(newline)
				(menu '())
				))
		     ((= n 1) (begin
				(display "\n\t Load roster from file: ")
				(let ((infile(open-input-file(read-line))))
					(let((inroster(read infile)))
						(close-input-port infile)
						(menu inroster)
					)
				)
				))
		     ((= n 2) (begin
				(display "\n\t Store roster to file: ")
				(let ((outfile(open-output-file(read-line))))
					(write roster outfile)
					(close-output-port outfile)
				)
				(menu roster)
				))
		     ((= n 3) (begin
				(display "\n\t Displaying roster, sorted by ID: ")
				(if(null? roster) (display "No records to display."))
				(view-roster (sort roster by-id) 1)
				(newline)
				(menu roster)
				)) 
		     ((= n 4) (begin
				(display "\n\t Enter a student name or ID: ")
				(stud-info(read-line) roster)
				(menu roster)
				)) 
		     ((= n 5) (begin
				(display "\n\t Add student to the roster\n ")
				(menu(cons(sec-input-stud 0 '()) roster))
				))
		     ((= n 6) (begin
				(display "\n\t Enter a student name or ID: ")
				(menu (stud-delete(read-line) roster))
				))
		     ((= n 7) (begin
				(display "\n\t Displaying roster, sorted by name: ")
				(if(null? roster) (display "No records to display."))
				(view-roster (sort roster by-name) 1)
				(newline)
				(menu roster)
				))
		     ((= n 8) (begin
				(display "\n\t Displaying roster, sorted by grade: ")
				(if(null? roster) (display "No records to display."))
				(view-roster (sort roster by-grade) 1)
				(newline)
				(menu roster)
				))
		     ((= n 9) (begin
				(display "\n\t Goodbye! \n")
				#t
				))
		      (else (begin
				(display "\n\t task no. ")
				(display n)
				(display " does not exist. \n\n")
				(menu roster)
				)
			)
		)
	)
)
				

(define menu
	(lambda(roster)
		(begin
			(display "\n\t Class roster management system \n")
			(display "\t ============================== \n")
			(display "\t\t MENU \n")
			(display "\t ============================== \n")
			(display "\t 0. Reset roster \n")
			(display "\t 1. Load roster from file \n")
			(display "\t 2. Store roster to file \n")
			(display "\t 3. Display roster sorted by ID \n")
			(display "\t 4. Diplay student info \n")
			(display "\t 5. Add a student to roster \n")
			(display "\t 6. Remove a student from roster \n")
			(display "\t 7. Display roster sorted by name \n")
			(display "\t 8. Display roster sorted by grade \n")
			(display "\t 9. Exit \n\n")
			(display "\t Enter your choice: ")
			(performtask (read) roster)
		)
	)
)
