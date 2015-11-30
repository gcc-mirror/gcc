/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -std=c99 -fipa-pta -fdump-tree-optimized" } */

#define N 2

int
foo (void)
{
  int a[N], b[N], c[N];
  int *ap = &a[0];
  int *bp = &b[0];
  int *cp = &c[0];

#pragma omp parallel for
  for (unsigned int idx = 0; idx < N; idx++)
    {
      ap[idx] = 1;
      bp[idx] = 2;
      cp[idx] = ap[idx];
    }

  return *cp;
}

/* { dg-final { scan-tree-dump-times "\\] = 1;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\] = 2;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\] = _\[0-9\]*;" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\] = " 3 "optimized" } } */

