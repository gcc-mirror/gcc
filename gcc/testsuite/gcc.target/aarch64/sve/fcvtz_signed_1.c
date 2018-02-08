/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 --save-temps" } */

#include <stdint.h>

void __attribute__ ((noinline, noclone))
vfcvtz_16 (int16_t *dst, _Float16 *src1, int size)
{
  for (int i = 0; i < size; i++)
    dst[i] = (int16_t) src1[i];
}

void __attribute__ ((noinline, noclone))
vfcvtz_32 (int32_t *dst, float *src1, int size)
{
  for (int i = 0; i < size; i++)
    dst[i] = (int32_t) src1[i];
}

void __attribute__ ((noinline, noclone))
vfcvtz_64 (int64_t *dst, double *src1, int size)
{
  for (int i = 0; i < size; i++)
    dst[i] = (int64_t) src1[i];
}

/* { dg-final { scan-assembler-times {\tfcvtzs\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcvtzs\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcvtzs\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d\n} 1 } } */
