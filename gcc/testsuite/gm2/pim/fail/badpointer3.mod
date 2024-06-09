MODULE badpointer3 ;

FROM SYSTEM IMPORT ADDRESS ;

CONST
   A = "hello" ;
   B = " world" ;
   Hello = A + B ;


PROCEDURE testproc (s: ADDRESS) ;
BEGIN
END testproc ;


PROCEDURE foo ;
BEGIN
   testproc (Hello)
END foo ;


BEGIN
   foo
END badpointer3.
