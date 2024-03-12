MODULE fooasm ;

VAR
   x: INTEGER ;

PROCEDURE test ;
BEGIN
   ASM("" : : "m"(x))
END test ;

BEGIN
   test
END fooasm.
