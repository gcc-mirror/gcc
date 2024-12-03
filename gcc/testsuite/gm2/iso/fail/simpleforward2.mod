MODULE simpleforward2 ;

PROCEDURE foo (c: CARDINAL) ;
BEGIN
END foo ;

PROCEDURE foo ; FORWARD ;

BEGIN
   foo (1)
END simpleforward2.
