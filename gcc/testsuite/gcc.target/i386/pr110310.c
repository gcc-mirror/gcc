/* { dg-do compile } */
/* { dg-options "-O3 -march=znver4 -fdump-tree-vect-optimized" } */

void foo (int * __restrict a, int *b)
{
  for (int i = 0; i < 20; ++i)
    a[i] = b[i] + 42;
}

/* We should vectorize the main loop with AVX512 and the epilog with SSE.  */

/* { dg-final { scan-tree-dump "optimized: loop vectorized using 64 byte vectors" "vect" } } */
/* { dg-final { scan-tree-dump "optimized: loop vectorized using 16 byte vectors" "vect" } } */
