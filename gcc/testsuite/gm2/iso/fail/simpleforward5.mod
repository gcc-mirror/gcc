MODULE simpleforward5 ;

PROCEDURE foo (c: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE foo (c: CARDINAL) : CARDINAL ; FORWARD ;

PROCEDURE foo (c: CARDINAL) ;
BEGIN
END foo ;

BEGIN
   foo (1)
END simpleforward5.
