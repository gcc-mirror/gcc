/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d --param riscv-autovec-preference=scalable -fdump-tree-optimized-details" } */

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
      a[i * 4 + 3] = c[i * 4 + 3] + d[i * 4 + 3];
      b[i * 4] = c[i * 4 + 2] + d[i * 4];
      b[i * 4 + 1] = c[i * 4 + 1] + d[i * 4 + 1];
      b[i * 4 + 2] = c[i * 4 + 3] + d[i * 4 + 2];
      b[i * 4 + 3] = c[i * 4 + 3] + d[i * 4 + 3];
    }
}

/* { dg-final { scan-tree-dump "\.VEC_PERM" "optimized" { xfail *-*-* } } } */
/* { dg-final { scan-assembler {\tvid\.v} { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not {\tvmul} } } */
