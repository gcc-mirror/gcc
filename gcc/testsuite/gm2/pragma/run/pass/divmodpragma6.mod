MODULE divmodpragma6 ;

(* A test module to test whether the dialect can be changed within a module.  *)


(*
   pim4 -
*)

PROCEDURE pim4 ;
<* TEST $(gm2.local.fpim2)
   & ERROR : "fpim2 should not be set."  *>
BEGIN
END pim4 ;


BEGIN
   pim4
END divmodpragma6.
