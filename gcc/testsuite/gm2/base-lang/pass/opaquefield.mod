IMPLEMENTATION MODULE opaquefield ;  (*!m2pim*)

FROM opaquestr IMPORT String, initString ;

TYPE
   Content = POINTER TO RECORD
                           next: String ;
                        END ;

PROCEDURE create () : Content ;
VAR
   c: Content ;
BEGIN
   c^.next := initString () ;
   RETURN c
END create ;


END opaquefield.
