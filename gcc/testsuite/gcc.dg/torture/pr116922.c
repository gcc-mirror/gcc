/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */
/* PR tree-optimization/116922 */


static int g;

void
foo (int c, double v, double *r)
{
b:
  do
    v /= g - v;
  while (c);
  *r = v;

  double x;
  foo (5, (double)0, &x);
}
