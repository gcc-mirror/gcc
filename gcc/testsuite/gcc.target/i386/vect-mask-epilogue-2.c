/* { dg-do compile } */
/* { dg-options "-O3 -march=znver5 -fdump-tree-vect-optimized" } */

void foo (double *a, double b, double c, int n, int m)
{
  for (int j = 0; j < m; ++j)
    for (int i = 0; i < n; ++i)
      a[j*n + i] = a[j*n + i] * b + c;
}

/* We do not want to use a masked epilogue for the inner loop as the next
   outer iteration will possibly immediately read from elements masked of
   the previous inner loop epilogue and that never forwards.  */
/* { dg-final { scan-tree-dump "epilogue loop vectorized using 32 byte vectors" "vect" } } */
