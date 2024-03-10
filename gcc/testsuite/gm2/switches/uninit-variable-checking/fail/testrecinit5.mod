MODULE testrecinit5 ;


TYPE
   color = RECORD
              r, g, b: CARDINAL ;
           END ;

   pixel = RECORD
              fg, bg: color ;
           END ;

PROCEDURE test ;
VAR
   p: pixel ;
BEGIN
   (* p.bg.b := 6 ; *)
   IF p.bg.b = 6
   THEN
   END
END test ;

BEGIN
   test
END testrecinit5.
