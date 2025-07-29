MODULE badvarname ;


PROCEDURE Foo ;
VAR
   end: CARDINAL ;
BEGIN
   end := 1
END Foo ;


BEGIN
   Foo
END badvarname.
