/* PR c/112571.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x" } */

enum X : typeof (enum X { A }); /* { dg-error "declared with but defined without fixed underlying type" } */
/* { dg-error "invalid 'enum' underlying type" "invalid" { target *-*-* } .-1 } */
