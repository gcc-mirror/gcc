MODULE testarrayunbounded6 ;


PROCEDURE foo (a: ARRAY OF ARRAY OF REAL) ;
BEGIN
END foo ;


VAR
   b: ARRAY [0..10], [0..5] OF CARDINAL ;
BEGIN
   foo (b)
END testarrayunbounded6.
