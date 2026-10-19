(* Test for NEW/DISPOSE with LongStorage.  *)
(* { dg-do compile { target *-*-* } } *)

MODULE largestore2 ;

FROM LongStorage IMPORT ALLOCATE, DEALLOCATE, Available ;
FROM libc IMPORT printf, exit ;
FROM SYSTEM IMPORT ADDRESS, BYTE ;

CONST
   G = 1024 * 1024 * 1024 ;
   Amount = G ;

TYPE
   Content = ARRAY [0..Amount] OF BYTE ;
   Buf = POINTER TO Content ;

VAR
   ptr: Buf ;
BEGIN
   IF Available (SIZE (Content))
   THEN
      printf ("2 GB available\n") ;
      NEW (ptr) ;
      IF ptr = NIL
      THEN
         printf (" .. but not allocated to this process\n") ;
         exit (1)
      END ;
      DISPOSE (ptr)
   ELSE
      printf ("2 GB unavailable\n")
   END
END largestore2.
