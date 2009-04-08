/* Test for constant expressions: __builtin_choose_expr.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic-errors" } */

#include <limits.h>

int a, b, c;

void
f (void)
{
  /* __builtin_choose_expr acts exactly like the chosen argument for
     all constant expression purposes.  */
  enum e {
    E1 = __builtin_choose_expr (1, 1, ++b)
  };
  /* The first argument to __builtin_choose_expr must be an integer
     constant expression.  */
  a = __builtin_choose_expr ((void *)0, b, c); /* { dg-error "constant" } */
  a = __builtin_choose_expr (0 * (INT_MAX + 1), b, c); /* { dg-warning "integer overflow in expression" } */
  /* { dg-error "overflow in constant expression" "constant" { target *-*-* } 21 } */
  a = __builtin_choose_expr (1 / 0, 0, 0); /* { dg-warning "division by zero" } */
  /* { dg-error "not a constant" "error" { target *-*-* } 23 } */
  a = __builtin_choose_expr ((1 ? 1 : a), b, c); /* { dg-error "constant" } */
}
