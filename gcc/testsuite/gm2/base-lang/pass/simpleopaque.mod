IMPLEMENTATION MODULE simpleopaque ;  (*!m2pim*)

TYPE
   List = POINTER TO RECORD
                        next: List ;
                     END ;


PROCEDURE init () : List ;
VAR
   l: List ;
BEGIN
   RETURN l
END init ;


PROCEDURE join (left, right: List) : List ;
VAR
   l: List ;
BEGIN
   l := left ;
   RETURN l
END join ;


PROCEDURE delete (VAR l: List) ;
BEGIN
   l := NIL
END delete ;


END simpleopaque.
