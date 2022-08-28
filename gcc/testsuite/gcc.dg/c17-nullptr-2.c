/* Test that we don't predefine `nullptr' pre-C2X.  */
/* { dg-do compile } */
/* { dg-options "-std=c17 -pedantic-errors" } */

int *
fn (int *p)
{
  p = nullptr; /* { dg-error "'nullptr' undeclared" } */
  return p;
}
