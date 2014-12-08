/* { dg-do compile } */
/* { dg-options "-fsanitize=integer-divide-by-zero" } */

/* TODO: We expect an error on the invalid case here, because that
   must be a constant-expression.  This will be fixed when we have
   proper delayed folding.  */

void
foo (int i)
{
  switch (i)
  case 0 * (1 / 0): /* { dg-warning "division by zero" } */
    ;  /* { dg-error "division by zero" "" { xfail *-*-* } 10 } */
}
