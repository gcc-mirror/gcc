IMPLEMENTATION MODULE simplelist ;  (*!m2pim*)


TYPE
   List = POINTER TO RECORD
                        next: List ;
                     END ;


PROCEDURE initList () : List ;
VAR
   l: List ;
BEGIN
   (* Ignore NEW (l) for now.  *)
   RETURN l
END initList ;


PROCEDURE concat (l: List) : List ;
BEGIN
   l^.next := initList () ;
   RETURN l
END concat ;


PROCEDURE addList (l: List) : List ;
BEGIN
   l^.next := initList () ;
   RETURN l
END addList ;


END simplelist.
