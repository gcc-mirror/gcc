MODULE testrecinit ;


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
   p.fg.r := 1 ;
   IF p.fg.g = 6   (* should catch error.  *)
   THEN
   END
END test ;

BEGIN
   test
END testrecinit.
