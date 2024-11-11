/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* { dg-additional-options "-O3" } */

void f(int n, int m, double *a)
{
  a = __builtin_assume_aligned (a, __BIGGEST_ALIGNMENT__);
  for (int i = 0; i < n; i++)
    for (int j = 0; j < m; j++)
      a[i] += 2*a[i] + j;
}

/* { dg-final { scan-tree-dump "OUTER LOOP VECTORIZED" "vect" } } */
