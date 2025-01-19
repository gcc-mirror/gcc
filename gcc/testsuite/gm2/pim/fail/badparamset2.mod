MODULE badparamset2 ;  

TYPE
   month = SET OF [1..12] ;
   day = SET OF [1..31] ;   


PROCEDURE foo (d: day) ;
BEGIN
END foo ;

VAR
   m: month ;
BEGIN
   foo (m)
END badparamset2.
