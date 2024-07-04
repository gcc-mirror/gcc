MODULE badset2 ;

FROM libc IMPORT printf ;

VAR
   s: SET OF [1..10] ;
   c: CARDINAL ;
BEGIN
   IF c # s
   THEN
      printf ("broken\n")
   END
END badset2.
