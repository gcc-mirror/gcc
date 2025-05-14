MODULE constarray2 ;

FROM libc IMPORT printf, exit ;

TYPE
   arraytype = ARRAY [0..11] OF CHAR ;
   
CONST
   Hello = "Hello" ;
   World = "World" ;
   HelloWorld = arraytype {Hello + " " + World} ;


(*
   Assert - 
*)

PROCEDURE Assert (result: BOOLEAN) ;
BEGIN
   IF NOT result
   THEN
      printf ("assertion failed\n") ;
      exit (1)
   END
END Assert ;


VAR
   ch: CHAR ;
BEGIN
   ch := HelloWorld[4] ;
   Assert (ch = 'o')
END constarray2.
