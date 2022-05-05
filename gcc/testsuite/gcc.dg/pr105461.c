/* { dg-do compile } */
/* { dg-options "-O2 -fassociative-math -fsignaling-nans -fvar-tracking  -w" } */

int
bar (float *x, int y)
{
  *x = y;

  return *x;
}

__attribute__ ((optimize ("O2"))) void
foo (float *x, int y)
{
  int a = bar (x, y);
}
