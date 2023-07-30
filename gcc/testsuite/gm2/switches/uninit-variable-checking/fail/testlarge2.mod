MODULE testlarge2 ;

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
   p.fg.g := 3 ;   (* Deliberate typo should be p.fg.b.  *)
   p.bg := p.fg ;  (* This should result in a warning.  *)
END test ;

BEGIN
   test
END testlarge2.
