MODULE badpointer2 ;

FROM DynamicStrings IMPORT String ;

CONST
   A = "hello" ;
   B = " world" ;
   Hello = A + B ;


PROCEDURE testproc (s: String) ;
BEGIN
END testproc ;


PROCEDURE foo ;
BEGIN
   testproc (Hello)
END foo ;


BEGIN
   foo
END badpointer2.
