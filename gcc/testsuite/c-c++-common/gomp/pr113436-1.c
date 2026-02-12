/* PR middle-end/113436 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-omplower" } */

#include <omp.h>

void
f()
{
  int A, B[10], *C;
  A = 5;
  C = (int *) __builtin_malloc (sizeof (int) * 10);
  for (int i = 0; i < 10; i++)
    B[i] = C[i] = i+5;
      
  #pragma omp target private(A) private(B) private(C) allocate(allocator(omp_low_lat_mem_alloc), align(128): A, B, C)
    {
      A = 99;
      for (int i = 0; i < 10; i++)
        B[i] = -i-23;
      C = &A;
    }
}

/* { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_GOMP_alloc \\\(128, 4, 5\\\);" "omplower" { target int32 } } } */
/* { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_GOMP_alloc \\\(128, 40, 5\\\);" "omplower" { target int32 } } } */
/* { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_GOMP_alloc \\\(128, 8, 5\\\);" "omplower" { target { lp64 || llp64 } } } } */
/* { dg-final { scan-tree-dump "\\\*D\\\.\[0-9\]\+ = 99;" "omplower" } } */
/* { dg-final { scan-tree-dump "\\\(\\\*D\\\.\[0-9\]\+\\\)\\\[i\\\] = D\\\.\[0-9\]\+;" "omplower" } } */
/* { dg-final { scan-tree-dump "\\\*D\\\.\[0-9\]\+ = D\\\.\[0-9\]\+;" "omplower" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\\(D\\\.\[0-9\]\+, 5\\\);" 3 "omplower" } } */
