IMPLEMENTATION MODULE opaqueuse ;  (*!m2pim*)

FROM opaqueparam IMPORT initList, add ;

PROCEDURE extending (l: List) : List ;
VAR
   n: List ;
BEGIN
   n := initList () ;
   add (n) ;
   RETURN n
END extending ;


END opaqueuse.
