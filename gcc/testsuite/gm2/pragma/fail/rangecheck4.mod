MODULE rangecheck4 ;

(* A test module to test whether the range check can be enabled
   and disabled within a module.  *)

<* gm2.local.fsoft_check_all=TRUE *>
(* Should cause the ERROR to occur below.  *)

PROCEDURE TestOn ;
<* TEST $(gm2.local.fsoft_check_all)
   & ERROR  *>
<* gm2.local.fsoft_check_all=FALSE *>
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   INC (i) ;
END TestOn ;


BEGIN
   TestOn ;
END rangecheck4.
