MODULE stressreturn ;
FROM Builtins IMPORT return_address;
FROM SYSTEM IMPORT ADDRESS;

VAR x: ADDRESS;

PROCEDURE test;
BEGIN
  ASM VOLATILE("" : "=m"(x) : "m"(return_address(0)) : );
END test;

BEGIN
   test
END stressreturn.
