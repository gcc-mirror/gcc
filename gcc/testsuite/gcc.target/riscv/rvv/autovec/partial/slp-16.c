/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -fdump-tree-optimized-details" } */

#include <stdint-gcc.h>

void
f (uint8_t *restrict a, uint8_t *restrict b, int n)
{
  for (int i = 0; i < n; ++i)
    {
      a[i * 8] = b[i * 8 + 3] + 1;
      a[i * 8 + 1] = b[i * 8 + 2] + 2;
      a[i * 8 + 2] = b[i * 8 + 1] + 3;
      a[i * 8 + 3] = b[i * 8 + 0] + 4;
      a[i * 8 + 4] = b[i * 8 + 7] + 5;
      a[i * 8 + 5] = b[i * 8 + 6] + 6;
      a[i * 8 + 6] = b[i * 8 + 5] + 7;
      a[i * 8 + 7] = b[i * 8 + 4] + 8;
    }
}

/* FIXME: Since we don't have VECT cost model yet, LOAD_LANES/STORE_LANES are chosen
   instead of SLP when rvv-autotec-max-lmul=m1.  */
/* { dg-final { scan-tree-dump-times "\.VEC_PERM" 1 "optimized" { xfail { any-opts "-mrvv-max-lmul=m1" } } } } */
/* { dg-final { scan-assembler {\tvid\.v} { xfail { any-opts "-mrvv-max-lmul=m1"} } } } */
/* { dg-final { scan-assembler-not {\tvmul} } } */
