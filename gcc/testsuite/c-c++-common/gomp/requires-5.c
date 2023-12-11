/* { dg-additional-options "-fdump-tree-original" }  */

#pragma omp requires atomic_default_mem_order(release)

int
foo (int x, int y)
{
  int z;

  #pragma omp atomic write
    x = y;

  #pragma omp atomic update
    x += 1;

  #pragma omp atomic read acquire
    z = x;
  return z;
}

/* { dg-final { scan-tree-dump "#pragma omp atomic release" "original" } } */
/* { dg-final { scan-tree-dump "#pragma omp atomic release" "original" } } */
/* { dg-final { scan-tree-dump "z = #pragma omp atomic read acquire" "original" } } */
