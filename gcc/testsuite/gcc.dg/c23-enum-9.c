/* PR c/112571.  */
/* { dg-do compile } */
/* { dg-options "-std=c23" } */

enum h : typeof (enum h { D }) { D }; /* { dg-error "declared with but defined without fixed underlying type" } */
/* { dg-error "invalid 'enum' underlying type" "invalid" { target *-*-* } .-1 } */
/* { dg-error "nested redefinition" "nested" { target *-*-* } .-2 } */
/* { dg-error "conflicting redefinition" "conflicting" { target *-*-* } .-3 } */
