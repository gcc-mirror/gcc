MODULE rangecheck3 ;

(* A test module to test whether the range check can be disabled
   and checked within a module.  *)

<* gm2.local.fsoft_check_all=FALSE *>
(* Should cause the error message below to occur.  *)

PROCEDURE TestOn ;
<* TEST $(gm2.local.fsoft_check_all)
   | ERROR : "gm2.local.fsoft_check_all should be set."  *>
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   INC (i) ;
END TestOn ;


BEGIN
   TestOn ;
END rangecheck3.
