IMPLEMENTATION MODULE simplelistiter ;  (*!m2pim*)

TYPE
   List = POINTER TO RECORD
                        next: List ;
                     END ;

PROCEDURE initList () : List ;
VAR
   l: List ;
BEGIN
   RETURN l
END initList ;


PROCEDURE items (l: List) : CARDINAL ;
(* VAR
   count: CARDINAL ; *)
BEGIN
(*   count := 0 ; *)
   WHILE l # NIL DO
      (* INC (count) ; *)
      l := l^.next
   END ;
   (* RETURN count *)
   RETURN 0
END items ;


END simplelistiter.
