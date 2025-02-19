/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake" } */

#define N 9

void
f (double x, double y, double *res)
{
  y = -y;
  for (int i = 0; i < N; ++i)
    {
      double tmp = y;
      y = x;
      x = tmp;
      res[i] = i;
    }
  res[N] = y * y;
  res[N + 1] = x;
}

/* { dg-final { scan-assembler-not "and\[lq\]?\[^\\n\]*-32,\[^\\n\]*sp" } } */
