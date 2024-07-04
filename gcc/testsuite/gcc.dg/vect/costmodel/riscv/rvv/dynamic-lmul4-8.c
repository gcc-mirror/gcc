/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3 -ftree-vectorize -mrvv-max-lmul=dynamic -mrvv-vector-bits=scalable -fselective-scheduling -fdump-tree-vect-details" } */
/* { dg-additional-options "-fno-schedule-insns -fno-schedule-insns2" } */

#include <stdint-gcc.h>

void
foo (uint8_t *restrict a, uint8_t *restrict b, int n)
{
  for (int i = 0; i < n; ++i)
    {
      a[i * 16] = b[i * 16 + 15] + 1;
      a[i * 16 + 1] = b[i * 16 + 14] + 2;
      a[i * 16 + 2] = b[i * 16 + 13] + 3;
      a[i * 16 + 3] = b[i * 16 + 12] + 4;
      a[i * 16 + 4] = b[i * 16 + 11] + 5;
      a[i * 16 + 5] = b[i * 16 + 10] + 6;
      a[i * 16 + 6] = b[i * 16 + 9] + 7;
      a[i * 16 + 7] = b[i * 16 + 8] + 8;
      
      a[i * 16 + 8] = b[i * 16 + 7] + 1;
      a[i * 16 + 9] = b[i * 16 + 6] + 2;
      a[i * 16 + 10] = b[i * 16 + 5] + 3;
      a[i * 16 + 11] = b[i * 16 + 4] + 4;
      a[i * 16 + 12] = b[i * 16 + 3] + 5;
      a[i * 16 + 13] = b[i * 16 + 2] + 6;
      a[i * 16 + 14] = b[i * 16 + 1] + 7;
      a[i * 16 + 15] = b[i * 16 + 0] + 8;
    }
}

/* { dg-final { scan-assembler {e8,m4} } } */
/* { dg-final { scan-assembler-times {csrr} 1 } } */
/* Since we don't support VLA SLP for LMUL = 8, dynamic LMUL cost model start from LMUL = 4.  */
/* { dg-final { scan-tree-dump-not "Preferring smaller LMUL loop because it has unexpected spills" "vect" } } */
/* { dg-final { scan-tree-dump-not "Maximum lmul = 8" "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 4" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 2" 1 "vect" } } */
