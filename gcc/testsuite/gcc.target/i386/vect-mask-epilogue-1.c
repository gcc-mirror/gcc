/* { dg-do compile } */
/* { dg-options "-O3 -march=znver5 -fdump-tree-vect-optimized" } */

void bar (double *a, double *b, double c, int n, int m)
{
  for (int j = 0; j < m; ++j)
    for (int i = 0; i < n; ++i)
      a[j*n + i] = b[j*n + i] + c;
}

/* { dg-final { scan-tree-dump "epilogue loop vectorized using masked 64 byte vectors" "vect" } } */
