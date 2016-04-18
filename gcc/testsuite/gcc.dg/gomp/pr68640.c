/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -fdump-tree-ealias-all" } */

#define N 1024

int
foo (int *__restrict__ ap)
{
  int *bp = ap;
#pragma omp parallel for
  for (unsigned int idx = 0; idx < N; idx++)
    ap[idx] = bp[idx];
}

/* { dg-final { scan-tree-dump-times "clique 1 base 1" 2 "ealias" } } */
/* { dg-final { scan-tree-dump-times "(?n)clique 1 base 0" 2 "ealias" } } */
