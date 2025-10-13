MODULE badtype ;

FROM SYSTEM IMPORT TSIZE ;

TYPE
   foo = CARDINAL ;
BEGIN
   IF TSIZE (Foo) = 1
   THEN
   END
END badtype.
