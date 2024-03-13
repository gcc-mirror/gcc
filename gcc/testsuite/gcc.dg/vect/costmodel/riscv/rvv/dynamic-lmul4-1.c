/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3 -ftree-vectorize -mrvv-max-lmul=dynamic -fdump-tree-vect-details" } */

#include <stdint-gcc.h>

void
foo (int32_t *__restrict a, int32_t *__restrict b, int32_t *__restrict c,
      int32_t *__restrict a2, int32_t *__restrict b2, int32_t *__restrict c2,
      int32_t *__restrict a3, int32_t *__restrict b3, int32_t *__restrict c3,
      int32_t *__restrict a4, int32_t *__restrict b4, int32_t *__restrict c4,
      int32_t *__restrict a5, int32_t *__restrict b5, int32_t *__restrict c5,
      int32_t *__restrict d, int32_t *__restrict d2, int32_t *__restrict d3,
      int32_t *__restrict d4, int32_t *__restrict d5, int n, int m)
{
  for (int i = 0; i < n; i++)
    {
      a[i] = b[i] + c[i];
      a2[i] = b2[i] + c2[i];
      a3[i] = b3[i] + c3[i];
      a4[i] = b4[i] + c4[i];
      a5[i] = a[i] + a4[i];
      d[i] = a[i] - a2[i];
      d2[i] = a2[i] * a[i];
      d3[i] = a3[i] * a2[i];
      d4[i] = a2[i] * d2[i];
      d5[i] = a[i] * a2[i] * a3[i] * a4[i] * d[i];
    }
}

/* { dg-final { scan-assembler {e32,m4} } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-tree-dump-times "Preferring smaller LMUL loop because it has unexpected spills" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 8" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 4" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 2" 1 "vect" } } */
