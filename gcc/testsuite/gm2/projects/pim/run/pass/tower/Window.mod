IMPLEMENTATION MODULE Window ;


FROM Screen IMPORT Width, Height ;



PROCEDURE ClipPoint (VAR x, y: CARDINAL ; Sx, Sy: CARDINAL ; VAR ok: BOOLEAN) ;
BEGIN
   IF (x>=Sx) AND (x<=Sx+Width) AND
      (y>=Sy) AND (y<=Sy+Height)
   THEN
      DEC(x, Sx) ;
      DEC(y, Sy) ;
      ok := TRUE
   ELSE
      ok := FALSE
   END
END ClipPoint ;


PROCEDURE Clip (VAR x1, y1, x2, y2: CARDINAL ;
                            Sx, Sy: CARDINAL ;
                            VAR ok: BOOLEAN) ;
BEGIN
   IF (Sx>x2) OR (Sx+Width<x1)
   THEN
      ok := FALSE ;
   ELSIF (Sy>y2) OR (Sy+Height<y1)
   THEN
      ok := FALSE
   ELSE
      ok := TRUE ;
      IF Sx>x1
      THEN
         x1 := 0
      ELSE
         DEC(x1, Sx)
      END ;
      IF Sy>y1
      THEN
         y1 := 0
      ELSE
         DEC(y1, Sy)
      END ;
      IF x2-Sx>Width
      THEN
         x2 := Width
      ELSE
         DEC(x2, Sx)
      END ;
      IF y2-Sy>Height
      THEN
         y2 := Height
      ELSE
         DEC(y2, Sy)
      END
   END
END Clip ;

      
END Window.
