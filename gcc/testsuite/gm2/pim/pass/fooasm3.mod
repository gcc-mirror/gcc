MODULE fooasm3 ;

VAR x: INTEGER;

PROCEDURE test;
BEGIN
  ASM("" : "=rm"(x));  (* x is an output.  *)
END test;

BEGIN
   test
END fooasm3.
