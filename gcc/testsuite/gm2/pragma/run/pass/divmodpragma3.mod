MODULE divmodpragma3 ;

(* A test module to test whether the dialect can be changed within a module.  *)

<* gm2.local.fpim2=TRUE *>    (* Default to the pim2 dialect.  *)


(*
   pim4 -
*)

PROCEDURE pim4 ;
<* TEST $(gm2.local.fpim2)
   | ERROR : "fpim2 should be set now."  *>
BEGIN
END pim4 ;


BEGIN
   pim4
END divmodpragma3.
