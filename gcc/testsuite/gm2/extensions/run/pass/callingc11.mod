MODULE callingc11 ;

FROM cvararg IMPORT funcptr ;
FROM SYSTEM IMPORT ADR ;
FROM strconst IMPORT WORLD ;

BEGIN
   IF funcptr (1, "hello", 5) = 1
   THEN
   END ;
   IF funcptr (1, "hello" + " ", 6) = 1
   THEN
   END ;
   IF funcptr (1, "hello" + " " + WORLD, 11) = 1
   THEN
   END
END callingc11.
