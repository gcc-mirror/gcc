MODULE testarrayunbounded2 ;


PROCEDURE foo (a: ARRAY OF ARRAY OF CARDINAL) ;
BEGIN

END foo ;


VAR
   b: ARRAY [0..10] OF CARDINAL ;
BEGIN
   foo (b)
END testarrayunbounded2.
