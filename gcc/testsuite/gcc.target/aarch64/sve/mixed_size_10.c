/* { dg-options "-O3 -msve-vector-bits=256 -fno-tree-loop-distribution" } */

float
f (float *restrict x, double *restrict y)
{
  float res = 0.0;
  for (int i = 0; i < 100; ++i)
    {
      res += x[i];
      y[i] += y[i - 4] * 11;
    }
  return res;
}
