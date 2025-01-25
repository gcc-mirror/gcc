
(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE badopaque2 ;  

FROM opaquedefs IMPORT OpaqueB ;

VAR
   b: OpaqueB ;
   c: CARDINAL ;
BEGIN
   c := 123 ;
   b^.width := c  (* { dg-bogus "unnamed" } *)
   (* { dg-error "cannot be dereferenced" "b^.width" { target *-*-* } 14 } *)   
   (* { dg-error "has no field" "no field" { target *-*-* } 14 } *)   
END badopaque2.
