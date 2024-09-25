MODULE testlibcstr ;

FROM SYSTEM IMPORT ADDRESS, ADR ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM libc IMPORT strtod, atof, printf, exit, snprintf ;
FROM DynamicStrings IMPORT String,
                           InitString, InitStringCharStar, Equal, Slice,
                           KillString ;

(*
   runtest -
*)

PROCEDURE runtest ;
CONST
   BufSpace = 100 ;
VAR
   s: String ;
   r: REAL ;
   a: ADDRESS ;
BEGIN
   r := atof (ADR ("3.14159")) ;
   ALLOCATE (a, BufSpace) ;
   snprintf (a, BufSpace, "%f", r) ;
   s := InitStringCharStar (a) ;
   IF NOT Equal (InitString ("3.14159"), Slice (s, 0, 7))
   THEN
      printf ("failed to convert 3.14159 to a REAL or string\n") ;
      exit (1)
   END ;
   DEALLOCATE (a, BufSpace) ;
   s := KillString (s)
END runtest ;


BEGIN
   runtest ;
   printf ("all tests passed!\n")
END testlibcstr.
