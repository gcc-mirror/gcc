/* { dg-do compile } */
/* { dg-options "-O -ffinite-math-only -fdump-tree-fre1" } */

extern double cos (double);
extern double tan (double);

int
f1 (double x, double y)
{
  double z1 = cos(y<10 ? x : tan(x<20 ? x : y));
  double z2 = cos(y<10 ? x : tan(x<20 ? x : y));
  return z1 == z2;
}

/* { dg-final { scan-tree-dump "return 1;" "fre1" } } */
