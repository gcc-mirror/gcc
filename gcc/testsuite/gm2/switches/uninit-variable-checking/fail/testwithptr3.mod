MODULE testwithptr3 ;

TYPE
   ptr = POINTER TO vec ;
   vec = RECORD
            x, y: CARDINAL ;
         END ;


PROCEDURE test ;
VAR
   p: ptr ;
BEGIN
   WITH p^ DO

   END
END test ;

BEGIN
   test
END testwithptr3.
