/* { dg-additional-options "-fdump-tree-original" }  */

#pragma omp requires atomic_default_mem_order(acquire)

int
bar (int a, int b)
{
  int c;

  #pragma omp atomic write release
    a = b;

  #pragma omp atomic update
    a += 1;

  #pragma omp atomic read
    c = a;
  return c;
}

/* { dg-final { scan-tree-dump "#pragma omp atomic release" "original" } } */
/* { dg-final { scan-tree-dump "#pragma omp atomic acquire" "original" } } */
/* { dg-final { scan-tree-dump "c = #pragma omp atomic read acquire" "original" } } */
