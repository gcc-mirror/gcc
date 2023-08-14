MODULE testsmallrec ;


TYPE
   vec = RECORD
            x, y: CARDINAL ;
         END ;

PROCEDURE test ;
VAR
   v: vec ;
BEGIN
   (* v.x := 1 ; *)
   v.y := 2 ;
   IF v.x = 1  (* This line should be the cause of a warning.  *)
   THEN
   END
END test ;

BEGIN
   test
END testsmallrec.
