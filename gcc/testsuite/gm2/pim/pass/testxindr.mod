MODULE testxindr ;  (*!m2iso+gm2*)

CONST
   NulName = 0 ;

TYPE
   Name = CARDINAL ;

   ptr = POINTER TO RECORD
                       n: Name ;
                    END ;

VAR
   p: ptr ;
BEGIN
   p^.n := NulName
END testxindr.
