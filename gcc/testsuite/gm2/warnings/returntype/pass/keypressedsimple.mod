MODULE keypressedsimple ;

FROM M2RTS IMPORT Halt ;
FROM Args IMPORT Narg ;

PROCEDURE KeyPressed () : BOOLEAN ;
BEGIN
   IF Narg () < 0
   THEN
      Halt ('no active status procedure', __FILE__, __FUNCTION__, __LINE__)
   ELSE
      RETURN FALSE
   END
END KeyPressed ;


BEGIN
   IF KeyPressed ()
   THEN
   END
END keypressedsimple.
