(* Test for > 4GB ALLOCATE.  *)
(* { dg-do compile { target *-*-* } } *)

MODULE largestore ;

FROM LongStorage IMPORT ALLOCATE, DEALLOCATE, Available ;
FROM libc IMPORT printf, exit ;
FROM SYSTEM IMPORT ADDRESS ;

CONST
   GB = 1024 * 1024 * 1024 ;
   Amount = 5 * GB ;

VAR
   start: ADDRESS ;
BEGIN
   IF Available (Amount)
   THEN
      printf ("%ld GB available\n", Amount DIV GB) ;
      ALLOCATE (start, Amount) ;
      IF start = NIL
      THEN
         printf (" .. but not allocated to this process\n") ;
         exit (1)
      END ;
      DEALLOCATE (start, Amount)
   ELSE
      printf ("%ld GB unavailable\n", Amount DIV GB)
   END
END largestore.
