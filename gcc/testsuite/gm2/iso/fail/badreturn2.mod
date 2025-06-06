MODULE badreturn2 ;


PROCEDURE foo ;
BEGIN
   RETURN 0
END foo ;


BEGIN
   foo
END badreturn2.
