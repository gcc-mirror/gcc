/* { dg-do compile } */
/* { dg-options "-O1 -march=armv8-a+fp8fma" } */

#include <arm_neon.h>

void foo(float16_t ap[20000], mfloat8x16_t b, mfloat8x16_t c, int br)
{
  float16x8_t a;
  a = vld1q_f16(ap);
  a = vmlalbq_f16_mf8_fpm(a, b, c, 13);
  vst1q_f16(ap, a);
  if (br)
    {
      a = vld1q_f16(ap + 8);
      a = vmlalbq_f16_mf8_fpm(a, b, c, 13);
      vst1q_f16(ap + 8, a);
      a = vld1q_f16(ap + 16);
      a = vmlalbq_f16_mf8_fpm(a, b, c, 13);
      vst1q_f16(ap + 16, a);
    }
  else
    {
      a = vld1q_f16(ap + 24);
      a = vmlalbq_f16_mf8_fpm(a, b, c, 13);
      vst1q_f16(ap + 24, a);
    }
  a = vld1q_f16(ap + 32);
  a = vmlalbq_f16_mf8_fpm(a, b, c, 13);
  vst1q_f16(ap + 32, a);
}

void bar(float16_t ap[20000], mfloat8x16_t b, mfloat8x16_t c, fpm_t mode, int br)
{
  float16x8_t a;
  a = vld1q_f16(ap);
  a = vmlalbq_f16_mf8_fpm(a, b, c, mode);
  vst1q_f16(ap, a);
  if (br)
    {
      a = vld1q_f16(ap + 8);
      a = vmlalbq_f16_mf8_fpm(a, b, c, mode);
      vst1q_f16(ap + 8, a);
      a = vld1q_f16(ap + 16);
      a = vmlalbq_f16_mf8_fpm(a, b, c, mode);
      vst1q_f16(ap + 16, a);
    }
  else
    {
      a = vld1q_f16(ap + 24);
      a = vmlalbq_f16_mf8_fpm(a, b, c, mode);
      vst1q_f16(ap + 24, a);
    }
  a = vld1q_f16(ap + 32);
  a = vmlalbq_f16_mf8_fpm(a, b, c, mode);
  vst1q_f16(ap + 32, a);
}

/* { dg-final { scan-assembler-times "msr\tfpmr" 2 } } */
