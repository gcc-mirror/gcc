/* { dg-do compile } */
/* { dg-options "-fsanitize=integer-divide-by-zero" } */

void
foo (int i)
{
  switch (i)
  case 0 * (1 / 0): /* { dg-warning "division by zero" } */
    ;  /* { dg-error "is not a constant.expression" "" { target *-*-* } .-1 } */
}
