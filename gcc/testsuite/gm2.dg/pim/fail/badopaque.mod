
(* { dg-do compile } *)
(* { dg-options "-g" } *)

MODULE badopaque ;  

FROM opaquedefs IMPORT OpaqueA ;

VAR
   a: OpaqueA ;
   c: CARDINAL ;
BEGIN
   c := 123 ;
   a^ := c     (* { dg-error "with an opaque type" }  *)
END badopaque.
