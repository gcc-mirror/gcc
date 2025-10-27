MODULE ReturnType ;

TYPE
   bar = POINTER TO CARDINAL ;


PROCEDURE foo (VAR value: bar) : bar ;
BEGIN
   RETURN value
END foo ;

VAR
   b: bar ;
BEGIN
   b := NIL ;
   b := foo (b)
END ReturnType.
