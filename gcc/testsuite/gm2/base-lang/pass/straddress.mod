IMPLEMENTATION MODULE straddress ;  (*!m2pim*)

FROM SYSTEM IMPORT ADDRESS, ADR ;


PROCEDURE open (a: ADDRESS) ;
BEGIN
END open ;

PROCEDURE Open (a: ARRAY OF CHAR) ;
BEGIN
   open (ADR (a))
END Open ;


END straddress.
