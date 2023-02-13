MODULE keypressedsimple ;

FROM M2RTS IMPORT Halt ;
FROM Args IMPORT Narg ;

PROCEDURE KeyPressed () : BOOLEAN ;
BEGIN
   IF Narg () < 0
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__, 'no active status procedure')
   ELSE
      RETURN FALSE
   END
END KeyPressed ;


BEGIN
   IF KeyPressed ()
   THEN
   END
END keypressedsimple.
