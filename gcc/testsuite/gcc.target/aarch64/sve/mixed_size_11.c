/* { dg-options "-O3 -msve-vector-bits=256 -fno-tree-loop-distribution" } */

float
f (float *restrict x, float *restrict y, long *indices)
{
  float res = 0.0;
  for (int i = 0; i < 100; ++i)
    {
      res += x[i - 4];
      x[i] = y[indices[i]];
    }
  return res;
}
