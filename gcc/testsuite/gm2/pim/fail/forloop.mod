MODULE forloop ;


PROCEDURE init ;
VAR
   i: INTEGER ;
   c: CARDINAL ;
BEGIN
   c := 10 ;
   FOR i := 0 TO c DO   (* INTEGER CARDINAL expression incompatible.  *)
   END
END init ;


BEGIN
   init
END forloop.
