/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64d -mabi=ilp32 -O3 -fno-vect-cost-model -mrvv-vector-bits=scalable -fdump-tree-vect-details" } */

#include "riscv_vector.h"

void
f (int32_t *__restrict f, int32_t *__restrict d, int n)
{
  for (int i = 0; i < n; ++i)
    {
      f[i * 2 + 0] = 1;
      f[i * 2 + 1] = 2;
      d[i] = 3;
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 1 "vect" } } */
