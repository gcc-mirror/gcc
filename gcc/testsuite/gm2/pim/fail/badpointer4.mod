MODULE badpointer4 ;

FROM DynamicStrings IMPORT String ;
FROM strconst IMPORT Hello ;


PROCEDURE testproc (s: String) ;
BEGIN
END testproc ;


PROCEDURE foo ;
BEGIN
   testproc (Hello)
END foo ;


BEGIN
   foo
END badpointer4.
