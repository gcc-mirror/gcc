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
   p.fg.g := 2 ;
   p.fg.b := 3 ;
   p.bg.r := 4 ;
   p.bg.g := 5 ;
   (* p.bg.b := 6 ; *)
   (* forget to initialize p.bg.b  *)
   IF p.bg.b = 6   (* should catch error.  *)
   THEN
   END
END test ;

BEGIN
   test
END testrecinit.
