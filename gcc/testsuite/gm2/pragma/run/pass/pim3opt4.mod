MODULE pim3opt4 ;

(* A test module to test whether the fpim3 flag can be set
   within a module.  *)

<* gm2.local.fpim3=TRUE *>

PROCEDURE TestOn ;
VAR
   i: CARDINAL ;
BEGIN
   i := 1
END TestOn ;


BEGIN
   TestOn ;
END pim3opt4.
