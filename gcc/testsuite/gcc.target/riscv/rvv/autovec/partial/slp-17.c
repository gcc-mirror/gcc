/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model -fdump-tree-optimized-details" } */

#include <stdint-gcc.h>

void
f (uint8_t *restrict a, uint8_t *restrict b,
   uint8_t *restrict c, uint8_t *restrict d,
   int n)
{
  for (int i = 0; i < n; ++i)
    {
      a[i * 8] = c[i * 8] + d[i * 8];
      a[i * 8 + 1] = c[i * 8] + d[i * 8 + 1];
      a[i * 8 + 2] = c[i * 8 + 2] + d[i * 8 + 2];
      a[i * 8 + 3] = c[i * 8 + 2] + d[i * 8 + 3];
      a[i * 8 + 4] = c[i * 8 + 4] + d[i * 8 + 4];
      a[i * 8 + 5] = c[i * 8 + 4] + d[i * 8 + 5];
      a[i * 8 + 6] = c[i * 8 + 6] + d[i * 8 + 6];
      a[i * 8 + 7] = c[i * 8 + 6] + d[i * 8 + 7];
      b[i * 8] = c[i * 8 + 1] + d[i * 8];
      b[i * 8 + 1] = c[i * 8 + 1] + d[i * 8 + 1];
      b[i * 8 + 2] = c[i * 8 + 3] + d[i * 8 + 2];
      b[i * 8 + 3] = c[i * 8 + 3] + d[i * 8 + 3];
      b[i * 8 + 4] = c[i * 8 + 5] + d[i * 8 + 4];
      b[i * 8 + 5] = c[i * 8 + 5] + d[i * 8 + 5];
      b[i * 8 + 6] = c[i * 8 + 7] + d[i * 8 + 6];
      b[i * 8 + 7] = c[i * 8 + 7] + d[i * 8 + 7];
    }
}

/* FIXME: Since we don't have VECT cost model yet, LOAD_LANES/STORE_LANES are chosen
   instead of SLP when rvv-autotec-max-lmul=m1.  */
/* { dg-final { scan-tree-dump-times "\.VEC_PERM" 2 "optimized" { xfail { any-opts "-mrvv-max-lmul=m1" } } } } */
/* { dg-final { scan-assembler {\tvid\.v} { xfail { any-opts "-mrvv-max-lmul=m1" } } } } */
/* { dg-final { scan-assembler-not {\tvmul} } } */
