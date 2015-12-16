/* { dg-options "-fdiagnostics-show-caret" } */

<<<<<<< HEAD /* { dg-error "conflict marker" } */
/* { dg-begin-multiline-output "" }
 <<<<<<< HEAD
 ^~~~~~~
   { dg-end-multiline-output "" } */

extern int some_var;

=======      /* { dg-error "conflict marker" } */
/* { dg-begin-multiline-output "" }
 =======
 ^~~~~~~
   { dg-end-multiline-output "" } */

extern short some_var; /* This line would lead to a warning due to the
			  duplicate name, but it is skipped when handling
			  the conflict marker.  */

>>>>>>> Some commit message  /* { dg-error "conflict marker" } */
/* { dg-begin-multiline-output "" }
 >>>>>>>
 ^~~~~~~
   { dg-end-multiline-output "" } */
