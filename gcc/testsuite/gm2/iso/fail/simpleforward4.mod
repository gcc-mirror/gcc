MODULE simpleforward4 ;


PROCEDURE foo () : CARDINAL ; FORWARD ;


PROCEDURE foo () ;
BEGIN
   RETURN 0
END foo ;


BEGIN
   IF foo () = 0
   THEN
   END
END simpleforward4.
