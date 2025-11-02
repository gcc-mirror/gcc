
(* { dg-do compile } *)
(* { dg-options "-g -c" } *)

MODULE badimport2 ;

FROM StrIO IMPORT Writestring ;
 (* { dg-error "error: In program module 'badimport2': unknown symbol 'Writestring', did you mean WriteString?" "Writestring" { target *-*-* } 7 } *)

BEGIN

END badimport2.
