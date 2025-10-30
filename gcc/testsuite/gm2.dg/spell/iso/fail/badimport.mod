
(* { dg-do compile } *)
(* { dg-options "-g -c" } *)

MODULE badimport ;

IMPORT ASCII ;
FROM StrIO IMPORT WriteString ;
FROM ASCIi IMPORT nul ;
 (* { dg-error "error: the file containing the definition module 'ASCIi' cannot be found, did you mean ASCII" "ASCIi" { target *-*-* } 9 } *)

BEGIN

END badimport.
