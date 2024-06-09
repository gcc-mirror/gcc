/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -fdump-tree-optimized-details -fno-vect-cost-model" } */

#include <stdint-gcc.h>

void
f (float *restrict a, float *restrict b,
   float *restrict c, float *restrict d,
   int n)
{
  for (int i = 0; i < n; ++i)
    {
      a[i * 4] = c[i * 4] + d[i * 4];
      a[i * 4 + 1] = c[i * 4] + d[i * 4 + 1];
      a[i * 4 + 2] = c[i * 4 + 2] + d[i * 4 + 2];
      a[i * 4 + 3] = c[i * 4 + 2] + d[i * 4 + 3];
      b[i * 4] = c[i * 4 + 1] + d[i * 4];
      b[i * 4 + 1] = c[i * 4 + 1] + d[i * 4 + 1];
      b[i * 4 + 2] = c[i * 4 + 3] + d[i * 4 + 2];
      b[i * 4 + 3] = c[i * 4 + 3] + d[i * 4 + 3];
    }
}

/* FIXME: Since we don't have VECT cost model yet, LOAD_LANES/STORE_LANES are chosen
   instead of SLP when rvv-autotec-max-lmul=m1 or m2.  */
/* { dg-final { scan-tree-dump-times "\.VEC_PERM" 2 "optimized" { xfail { any-opts "-mrvv-max-lmul=m1" "-mrvv-max-lmul=m2" } } } } */
/* { dg-final { scan-assembler {\tvid\.v} { xfail { any-opts "-mrvv-max-lmul=m1" "-mrvv-max-lmul=m2" } } } } */
/* { dg-final { scan-assembler-not {\tvmul} } } */
