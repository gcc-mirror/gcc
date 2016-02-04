int p;

<<<<<<< HEAD /* { dg-error "conflict marker" } */
extern int some_var;
=======      /* { dg-error "conflict marker" } */
extern short some_var; /* This line would lead to a warning due to the
			  duplicate name, but it is skipped when handling
			  the conflict marker.  */
>>>>>>> Some commit message  /* { dg-error "conflict marker" } */

int q;
