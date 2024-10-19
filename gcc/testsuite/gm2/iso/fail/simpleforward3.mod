MODULE simpleforward3 ;

PROCEDURE foo (c: CARDINAL) ;
BEGIN
END foo ;

PROCEDURE foo (c: CARDINAL) : CARDINAL ; FORWARD ;

BEGIN
   foo (1)
END simpleforward3.
