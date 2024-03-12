MODULE testsmallrec ;


TYPE
   vec = RECORD
            x, y: CARDINAL ;
         END ;

PROCEDURE test ;
VAR
   v: vec ;
BEGIN
   v.x := 1 ;
   v.y := 2 ;
   IF v.x = 1
   THEN
   END
END test ;

BEGIN
   test
END testsmallrec.
