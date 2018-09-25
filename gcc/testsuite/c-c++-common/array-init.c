/* { dg-do compile } */
/* { dg-prune-output "sorry, unimplemented: non-trivial designated initializers not supported" } */
/* { dg-prune-output "all initializer clauses should be designated" } */

char x[] = { [-1] = 1, 2, 3 }; /* { dg-error "array index in initializer exceeds array bounds" "" { target c } } */
