// { dg-do compile }
// { dg-options "-O2 -ffinite-math-only -fno-trapping-math -fno-tree-dominator-opts" }

void
foo (float x, int *y)
{
  int i;
  float sum2 = 0.0;

  for (i = 0; i < *y; ++i)
    sum2 += x;

  sum2 = 1.0 / sum2;
  if (sum2 * 0.0 < 5.E-5)
    *y = 0;
}
