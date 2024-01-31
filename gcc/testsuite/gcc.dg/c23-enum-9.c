/* PR c/112571.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x" } */

enum h : typeof (enum h { D }) { D }; /* { dg-error "declared with but defined without fixed underlying type" } */
/* { dg-error "invalid 'enum' underlying type" "invalid" { target *-*-* } .-1 } */
/* { dg-error "redeclaration of 'enum h'" "enumeration" { target *-*-* } .-2 } */
/* { dg-error "redeclaration of enumerator" "enumerator" { target *-*-* } .-3 } */
