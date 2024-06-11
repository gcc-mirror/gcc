IMPLEMENTATION MODULE opaqueparam ;  (*!m2pim*)

TYPE
   List = POINTER TO RECORD
                        next: List ;
                     END ;


PROCEDURE local (l: List) : List ;
BEGIN
   RETURN l
END local ;


PROCEDURE add (l: List) ;
BEGIN
   IF l = NIL
   THEN
      l^.next := local (initList ())
   END
END add ;


PROCEDURE initList () : List ;
VAR
   l: List ;
BEGIN
   RETURN l
END initList ;


END opaqueparam.
