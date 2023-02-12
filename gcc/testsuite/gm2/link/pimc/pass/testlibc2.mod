(* testlibc2 test for the presence of printf and the ability to pass varargs.  *)

MODULE testlibc2 ;

FROM libc IMPORT printf ;

VAR
   i: INTEGER ;
   r: REAL ;
BEGIN
   printf ("hello world\n");
   i := 12 ;
   printf ("int value of 12 = %d\n", i);
   r := 3.14159 ;  (* REAL is a double.  *)
   printf ("REAL approx of pi = %g\n", r);
END testlibc2.
