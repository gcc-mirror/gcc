/* Test auto with implicit int for C90.  */
/* { dg-do compile } */
/* { dg-options "-std=c90 -pedantic-errors" } */

void
f (void)
{
  /* This should have type int following C90 rules, whereas in C2x it
     would have type double.  */
  auto x = 1.5;
  int *p = &x;
}
