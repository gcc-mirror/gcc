MODULE conststrarray2 ;

FROM libc IMPORT printf, exit ;

CONST
   HelloWorld = Hello + " " + World ;
   Hello = "Hello" ;
   World = "World" ;


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
END conststrarray2.
