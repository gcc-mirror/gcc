MODULE simpleforward6 ;

PROCEDURE foo () : CARDINAL ;
BEGIN
   RETURN 0
END foo ;

PROCEDURE foo () : CARDINAL ; FORWARD ;

BEGIN
   IF foo () = 0
   THEN
   END
END simpleforward6.
