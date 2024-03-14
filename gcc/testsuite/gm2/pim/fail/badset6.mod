MODULE badset6 ;

FROM libc IMPORT printf ;

TYPE
   set = SET OF [1..10] ;

PROCEDURE Init (s: set) ;
VAR
   c: CARDINAL ;
BEGIN
   IF c > s
   THEN
      printf ("broken\n")
   ELSE
      printf ("broken\n")
   END
END Init ;


BEGIN
   Init (set {5,6})
END badset6.
