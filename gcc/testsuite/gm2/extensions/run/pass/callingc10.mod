MODULE callingc10 ;

FROM cvararg IMPORT funcptr ;
FROM SYSTEM IMPORT ADR ;

BEGIN
   IF funcptr (1, "hello", 5) = 1
   THEN
   END ;
   IF funcptr (1, "hello" + " ", 6) = 1
   THEN
   END ;
   IF funcptr (1, "hello" + " " + "world", 11) = 1
   THEN
   END
END callingc10.
