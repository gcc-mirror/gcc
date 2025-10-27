MODULE testxindr3 ;  (*!m2iso+gm2*)

CONST
   NulName = 0 ;

PROCEDURE set (VAR n: CARDINAL) ;
BEGIN
   n := NulName
END set ;

VAR
   n: CARDINAL ;
BEGIN
   set (n)
END testxindr3.
