MODULE rangecheck ;

(* A test module to test whether the range check can be enabled
   and disabled within a module.  *)

<* gm2.local.fsoft_check_all=FALSE *>
(* Will cause both tests below to generate an ERROR.  *)


PROCEDURE TestOn ;
<* TEST $(gm2.local.fsoft_check_all)
   | ERROR : "gm2.local.fsoft_check_all should be set."  *>
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   INC (i) ;
END TestOn ;


PROCEDURE TestOff ;
<* gm2.local.fsoft_check_all=FALSE *>
<* TEST $(gm2.local.fsoft_check_all)
   & ERROR : "gm2.local.fsoft_check_all should not be set."  *>
VAR
   i: CARDINAL ;
BEGIN
   i := 0 ;
   DEC (i) ;
END TestOff ;


BEGIN
   TestOn ;
   TestOff
END rangecheck.
