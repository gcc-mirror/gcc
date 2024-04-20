MODULE castproctype ;

IMPORT SYSTEM ;

TYPE
   foo3 = PROCEDURE (CARDINAL, INTEGER, CHAR) ;
   foo2 = PROCEDURE (CARDINAL, INTEGER) ;

CONST
   bar = SYSTEM.CAST (foo2, NIL) ;

VAR
   p2: foo2 ;
   p3: foo3 ;
BEGIN
   IF p2 = p3
   THEN
   END
END castproctype.
