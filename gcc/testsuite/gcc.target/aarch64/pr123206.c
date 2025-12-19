/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+nosimd" } */
#include <arm_neon.h>
mfloat8x8_t foo(uint8x8_t a, mfloat8x8_t b, mfloat8x8_t c)
{
  return vbsl_mf8(a,b,c); /* { dg-error "ACLE function 'vbsl_mf8' requires ISA extension 'simd'" } */
}
