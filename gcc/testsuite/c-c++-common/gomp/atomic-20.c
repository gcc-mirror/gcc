/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-final { scan-tree-dump-times "omp atomic release" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "omp atomic seq_cst" 3 "original" } } */
/* { dg-final { scan-tree-dump-times "omp atomic read seq_cst" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "omp atomic capture seq_cst" 1 "original" } } */

int i, j, k, l, m, n;

void
foo ()
{
  int v;
  #pragma omp atomic release
  i = i + 1;
  #pragma omp requires atomic_default_mem_order (seq_cst)
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
