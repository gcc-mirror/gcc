/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-final { scan-tree-dump-times "omp atomic release" 4 "original" } } */
/* { dg-final { scan-tree-dump-times "omp atomic read acquire" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "omp atomic capture acq_rel" 1 "original" } } */

int i, j, k, l, m, n;

void
foo ()
{
  #pragma omp atomic release
  i = i + 1;
}

#pragma omp requires atomic_default_mem_order (acq_rel)

void
bar ()
{
  int v;
  #pragma omp atomic
  j = j + 1;
  #pragma omp atomic update
  k = k + 1;
  #pragma omp atomic read
  v = l;
  #pragma omp atomic write
  m = v;
  #pragma omp atomic capture
  v = n = n + 1;
}
