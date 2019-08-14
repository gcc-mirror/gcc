/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#include <stdint.h>

void __attribute__ ((noinline, noclone))
clrsb_32 (unsigned int *restrict dst, uint32_t *restrict src, int size)
{
  for (int i = 0; i < size; ++i)
    dst[i] = __builtin_clrsb (src[i]);
}

void __attribute__ ((noinline, noclone))
clrsb_64 (unsigned int *restrict dst, uint64_t *restrict src, int size)
{
  for (int i = 0; i < size; ++i)
    dst[i] = __builtin_clrsbll (src[i]);
}

/* { dg-final { scan-assembler-times {\tcls\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcls\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuzp1\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
