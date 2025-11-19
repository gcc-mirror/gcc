/* { dg-do compile } */
/* { dg-options "-O3 -mavx512f -mprefer-vector-width=512 --param vect-partial-vector-usage=1 -fdump-tree-vect-optimized" } */

double foo (double *a, long long *mask, int n)
{
  double sum = 0.0;
  for (int i = 0; i < n; ++i)
    {
      double val;
      if (mask[i])
        val = a[i];
      else
        val = -0.0;
      sum = sum + val;
    }
  return sum;
}

/* { dg-final { scan-tree-dump "optimized: loop vectorized using 64 byte vectors" "vect" } } */
/* { dg-final { scan-tree-dump "optimized: epilogue loop vectorized using masked 64 byte vectors" "vect" } } */
