/* Test for constant expressions: C90 aggregate initializers requiring
   constant expressions.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors -O2" } */

#include <float.h>
#include <limits.h>

double atan(double);

struct s { double d; };
struct t { int i; };

void
f (void)
{
  /* As in PR 14649 for static initializers.  */
  struct s a = { atan (1.0) }; /* { dg-error "is not a constant expression|near initialization" } */
  /* Overflow.  */
  struct t b = { INT_MAX + 1 }; /* { dg-warning "integer overflow in expression" } */
  /* { dg-error "overflow in constant expression" "constant" { target *-*-* } .-1 } */
  struct t c = { DBL_MAX }; /* { dg-warning "overflow in implicit constant conversion" } */
  /* { dg-error "overflow in constant expression" "constant" { target *-*-* } .-1 } */
  /* Bad operator outside sizeof.  */
  struct s d = { 1 ? 1.0 : atan (a.d) }; /* { dg-error "is not a constant expression|near initialization" } */
}
