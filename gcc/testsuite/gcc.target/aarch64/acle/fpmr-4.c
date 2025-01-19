/* { dg-do compile } */
/* { dg-options "-O1 -march=armv8-a+fp8fma" } */

#include <arm_neon.h>

void baz(float16_t ap[20000], mfloat8x16_t b, mfloat8x16_t c)
{
  float16x8_t x = vld1q_f16(ap + 1);
  x = vmlalbq_f16_mf8_fpm(x, b, c, 13);
  vst1q_f16(ap + 1, x);
  for (int i = 0; i < 10; i++)
    {
      float16x8_t a = vld1q_f16(ap + 16*i);
      a = vmlalbq_f16_mf8_fpm(a, b, c, 13);
      vst1q_f16(ap + 16*i, a);
      a = vld1q_f16(ap + 16*i + 8);
      a = vmlalbq_f16_mf8_fpm(a, b, c, 865);
      vst1q_f16(ap + 16*i+8, a);
    }
}

/* { dg-final { scan-assembler-times "msr\tfpmr" 3 } } */
/* { dg-final { scan-assembler "msr\tfpmr.*\n\tb\t" } } */
