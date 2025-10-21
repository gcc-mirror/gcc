/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */

/* Check that the OMP_STRUCTURED_BLOCK that wraps intervening code is accepted.
 */

void f(int *A, int *B, int *C)
{
  #pragma omp for simd collapse(2)
  for (int i=0; i < 1; i++) {
    for (int j=0; j < 1; j++)
      A[i] += B[j];
    C[i] = 4;
  }
}

/* { dg-final { scan-tree-dump "#pragma omp __structured_block" "original" } } */
