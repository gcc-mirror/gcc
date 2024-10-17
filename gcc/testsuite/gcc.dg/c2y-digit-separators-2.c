/* Test C2Y digit separators.  Invalid usages.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

void
tf (void)
{
  int i;
  i = 0o'0; /* { dg-error "digit separator after base indicator" } */
  i = 0O'7; /* { dg-error "digit separator after base indicator" } */
}
