MODULE callingc10 ;

FROM cvararg IMPORT funcptr ;
FROM SYSTEM IMPORT ADR ;

BEGIN
   IF funcptr (INTEGER (1), "hello", INTEGER (5)) = INTEGER (1)
   THEN
   END ;
   IF funcptr (INTEGER (1), "hello" + " ", INTEGER (6)) = INTEGER (1)
   THEN
   END ;
   IF funcptr (INTEGER (1), "hello" + " " + "world", INTEGER (11)) = INTEGER (1)
   THEN
   END
END callingc10.
