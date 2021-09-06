/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vectorize -fopenmp -std=c99 -fipa-pta -fdump-tree-optimized" } */

#define N 2

int
foo (void)
{
  int a[N], c[N];
  int *ap = &a[0];
  int *bp = &a[0];
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

/* { dg-final { scan-tree-dump-times "\\] = 1;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\] = 2;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\] = _\[0-9\]*;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\] = " 3 "optimized" } } */
