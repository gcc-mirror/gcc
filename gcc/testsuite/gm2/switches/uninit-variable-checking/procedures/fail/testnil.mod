MODULE testnil ;


PROCEDURE test ;
VAR
   p: POINTER TO CARDINAL ;
BEGIN
   p := NIL ;
   IF p^ = 1
   THEN
   END
END test ;


BEGIN
   test
END testnil.
