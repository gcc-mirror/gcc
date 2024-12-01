MODULE testarrayunbounded5 ;


PROCEDURE foo (a: ARRAY OF ARRAY OF REAL) ;
BEGIN
END foo ;


VAR
   b: ARRAY [0..10] OF REAL ;
BEGIN
   foo (b)
END testarrayunbounded5.
