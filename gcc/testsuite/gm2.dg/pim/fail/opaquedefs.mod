(* { dg-do compile } *)
(* { dg-options "-g -c" } *)

IMPLEMENTATION MODULE opaquedefs ;

TYPE
   OpaqueA = POINTER TO CARDINAL ;
   OpaqueB = POINTER TO RECORD
                           width : CARDINAL ;
                           height: CARDINAL ;
                        END ;

END opaquedefs.
