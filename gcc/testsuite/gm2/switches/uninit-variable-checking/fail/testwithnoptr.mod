MODULE testwithnoptr ;

TYPE
   Vec =  RECORD
             x, y: CARDINAL ;
          END ;


(*
   test -
*)

PROCEDURE test ;
VAR
   p: Vec ;
BEGIN
   WITH p DO
      x := 1 ;
      x := 2   (* deliberate typo - should be y  *)
   END ;
   IF p.y = 2
   THEN
   END
END test ;


BEGIN
   test
END testwithnoptr.
