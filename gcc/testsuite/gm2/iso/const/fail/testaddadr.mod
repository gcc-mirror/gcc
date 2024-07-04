MODULE testaddadr ;

IMPORT SYSTEM ;

CONST
   foo = SYSTEM.ADDADR (ADR (a) + ADR (b)) ;

VAR
   a, b: CARDINAL ;
BEGIN

END testaddadr.
