IMPLEMENTATION MODULE stringaddr2 ;

FROM SYSTEM IMPORT ADDRESS, ADR ;

PROCEDURE foo ;
BEGIN
   a := ADR("hello world")
END foo ;

VAR
   a: ADDRESS ;
BEGIN

END stringaddr2.
