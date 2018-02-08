/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 --save-temps" } */

#include <stdint.h>

void __attribute__ ((noinline, noclone))
vcvtf_16 (_Float16 *dst, int16_t *src1, int size)
{
  for (int i = 0; i < size; i++)
    dst[i] = (_Float16) src1[i];
}

void __attribute__ ((noinline, noclone))
vcvtf_32 (float *dst, int32_t *src1, int size)
{
  for (int i = 0; i < size; i++)
    dst[i] = (float) src1[i];
}

void __attribute__ ((noinline, noclone))
vcvtf_64 (double *dst, int64_t *src1, int size)
{
  for (int i = 0; i < size; i++)
    dst[i] = (double) src1[i];
}

/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d\n} 1 } } */
