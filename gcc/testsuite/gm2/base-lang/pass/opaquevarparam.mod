IMPLEMENTATION MODULE opaquevarparam ;

TYPE
   Opaque = POINTER TO RECORD
                          next: Opaque ;
                       END ;

PROCEDURE del (VAR o: Opaque) ;
BEGIN
   IF o # NIL
   THEN
      del (o^.next)
   END
END del ;

END opaquevarparam.
