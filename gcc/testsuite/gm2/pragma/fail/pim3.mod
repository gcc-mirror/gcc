MODULE pim3 ;

(* A test module to test whether the fpim3 can be disabled
   and checked within a module.  *)

<* gm2.local.fpim3=FALSE *>


PROCEDURE TestOn ;
<* TEST $(gm2.local.fpim3)
   | ERROR : "gm2.local.fpim3 should be set."  *>
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   INC (i) ;
END TestOn ;


BEGIN
   TestOn ;
END pim3.
