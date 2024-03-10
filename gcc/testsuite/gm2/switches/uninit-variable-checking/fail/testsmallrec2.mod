MODULE testsmallrec2 ;


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
   WITH v DO
      IF x = 1
      THEN
      END
   END
END test ;

BEGIN
   test
END testsmallrec2.
