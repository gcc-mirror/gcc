/* Test C2Y _Generic features: VM types still not allowed.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

void
f (int i)
{
  (void) _Generic (i, int : 1, int (*)[i] : 2); /* { dg-error "variable length" } */
}
