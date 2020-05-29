/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize -save-temps --param aarch64-sve-compare-costs=0" } */

#include <stdint.h>

void
f (uint32_t *restrict a, int8_t *restrict b, int8_t mask, int n)
{
  for (int i = 0; i < n; ++i)
    a[i] += (int8_t) (b[i] | mask);
}

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+\]\n} 1 { xfail ilp32 } } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, #1, mul vl\]\n} 1 { xfail ilp32 } } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, #2, mul vl\]\n} 1 { xfail ilp32 } } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, #3, mul vl\]\n} 1 { xfail ilp32 } } } */
