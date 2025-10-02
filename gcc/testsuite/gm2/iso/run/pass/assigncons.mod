MODULE assigncons ;


TYPE
   rec = RECORD
            x, y: CARDINAL ;
         END ;

CONST
   z = rec {1, 2} ;


PROCEDURE Init ;
VAR
   r: rec ;
BEGIN
   r := z
END Init ;


BEGIN
   Init
END assigncons.
