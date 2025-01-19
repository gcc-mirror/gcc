MODULE forloopbyvar ;


PROCEDURE foo ;
VAR
   i, n: CARDINAL ;
   s   : CARDINAL ;
BEGIN
   s := 1 ;
   FOR i := 1 TO 10 BY s DO
   END
END foo ;

BEGIN
   foo
END forloopbyvar.
