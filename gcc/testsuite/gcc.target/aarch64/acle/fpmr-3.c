/* { dg-do compile } */
/* { dg-options "-O1 -march=armv8-a+fp8fma" } */

#include <arm_neon.h>

void foo(float16_t ap[20000], mfloat8x16_t b, mfloat8x16_t c, fpm_t mode)
{
  float16x8_t x = vld1q_f16(ap + 1);
  x = vmlalbq_f16_mf8_fpm(x, b, c, mode);
  vst1q_f16(ap + 1, x);
  for (int i = 0; i < 103; i++)
    {
      float16x8_t a = vld1q_f16(ap + 8*i);
      a = vmlalbq_f16_mf8_fpm(a, b, c, mode);
      vst1q_f16(ap + 8*i, a);
    }
}
/* { dg-final { scan-assembler-times "msr\tfpmr" 1 } } */
