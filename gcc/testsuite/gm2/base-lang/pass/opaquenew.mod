IMPLEMENTATION MODULE opaquenew ;  (*!m2pim*)

TYPE
   List = POINTER TO RECORD
                        next: List ;
                     END ;


PROCEDURE dupList (l: List) : List ;
VAR
   n: List ;
BEGIN
   (* NEW (n) *)
   n^ := l^ ;
   RETURN n
END dupList ;

END opaquenew.
