/* Test for constant expressions: overflow and constant expressions
   with -fwrapv: overflows still count as such for the purposes of
   constant expressions even when they have defined values at
   runtime.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors -fwrapv" } */

#include <limits.h>

enum e {
  E0 = 0 * (INT_MAX + 1), /* { dg-warning "integer overflow in expression" } */
  /* { dg-error "overflow in constant expression" "constant" { target *-*-* } .-1 } */
  E1 = 0 * (INT_MIN / -1), /* { dg-warning "integer overflow in expression" } */
  /* { dg-error "overflow in constant expression" "constant" { target *-*-* } .-1 } */
  E2 = 0 * (INT_MAX * INT_MAX), /* { dg-warning "integer overflow in expression" } */
  /* { dg-error "overflow in constant expression" "constant" { target *-*-* } .-1 } */
  E3 = 0 * (INT_MIN - 1), /* { dg-warning "integer overflow in expression" } */
  /* { dg-error "overflow in constant expression" "constant" { target *-*-* } .-1 } */
  E4 = 0 * (unsigned)(INT_MIN - 1), /* { dg-warning "integer overflow in expression" } */
  /* { dg-error "overflow in constant expression" "constant" { target *-*-* } .-1 } */
  E5 = 0 * -INT_MIN, /* { dg-warning "integer overflow in expression" } */
  /* { dg-error "overflow in constant expression" "constant" { target *-*-* } .-1 } */
  E6 = 0 * !-INT_MIN, /* { dg-warning "integer overflow in expression" } */
  /* { dg-error "not an integer constant" "constant" { target *-*-* } .-1 } */
  E7 = INT_MIN % -1 /* { dg-warning "16:integer overflow in expression" } */
  /* { dg-error "1:overflow in constant expression" "constant" { target *-*-* } .+1 } */
};
