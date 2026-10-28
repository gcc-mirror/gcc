MODULE pim3opt2 ;

(* A test module to test whether the fpim3 flag can be queried
   and enabled within a module.  *)

<* gm2.local.fpim3=TRUE *>

PROCEDURE TestOn ;
<* TEST $(gm2.local.fpim3)
   & INFO : "gm2.local.fpim3 is set correctly."  *>
VAR
   i: CARDINAL ;
BEGIN
   i := 1
END TestOn ;


BEGIN
   TestOn ;
END pim3opt2.
