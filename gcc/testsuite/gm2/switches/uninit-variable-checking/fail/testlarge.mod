MODULE testlarge ;

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
   (* p.fg.b := 3 ; *)
   p.bg := p.fg ;  (* this should result in a warning.  *)
   IF p.bg.b = 6
   THEN
   END
END test ;

BEGIN
   test
END testlarge.
