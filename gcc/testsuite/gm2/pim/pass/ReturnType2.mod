MODULE ReturnType2 ;

TYPE
   bar = POINTER TO RECORD
                       field: CARDINAL ;
                    END ;


PROCEDURE foo (VAR value: bar) : bar ;
BEGIN
   RETURN value
END foo ;

VAR
   b: bar ;
BEGIN
   b := NIL ;
   b := foo (b)
END ReturnType2.
