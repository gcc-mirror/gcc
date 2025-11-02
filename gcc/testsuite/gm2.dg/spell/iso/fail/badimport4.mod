
(* { dg-do compile } *)
(* { dg-options "-g -c" } *)

MODULE badimport4 ;

CONST
   Foo = 42 ;

MODULE inner ;
IMPORT foo ;
 (* { dg-error "error: In inner module 'inner': unknown symbol 'foo', did you mean Foo?" "foo" { target *-*-* } 11 } *)
END inner ;


BEGIN
END badimport4.
