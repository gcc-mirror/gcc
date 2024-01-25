MODULE badpointer ;

FROM DynamicStrings IMPORT String ;

CONST
   Hello = "hello world" ;


PROCEDURE testproc (s: String) ;
BEGIN
END testproc ;


PROCEDURE foo ;
BEGIN
   testproc (Hello)
END foo ;


BEGIN
   foo
END badpointer.
