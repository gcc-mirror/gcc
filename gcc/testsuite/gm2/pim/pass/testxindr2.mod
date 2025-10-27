MODULE testxindr2 ;  (*!m2iso+gm2*)

CONST
   NulName = 0 ;
TYPE
   Name = CARDINAL ;

PROCEDURE set (VAR n: Name) ;
BEGIN
   n := NulName
END set ;

VAR
   n: Name ;
BEGIN
   set (n)
END testxindr2.
