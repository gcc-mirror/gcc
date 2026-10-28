MODULE pim3opt3 ;

(* A test module to test whether the fpim3 flag can be queried
   and enabled within a module.  *)

<* gm2.local.fpim3=FALSE *>

PROCEDURE TestOn ;
<* gm2.local.fpim3=FALSE *>
<* TEST $(gm2.local.fpim3)
   & ERROR : "gm2.local.fpim3 should not be set."  *>
VAR
   i: CARDINAL ;
BEGIN
   i := 1
END TestOn ;


BEGIN
   TestOn ;
END pim3opt3.
