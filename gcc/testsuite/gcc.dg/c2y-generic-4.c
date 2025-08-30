/* Test C2Y _Generic features: VM types allowed. Warn for -Wc23-c2y-compat  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors -Wc23-c2y-compat" } */

void
f (int i)
{
  (void) _Generic (i, int : 1, int (*)[i] : 2);	/* { dg-warning "variably-modified type" } */
}

