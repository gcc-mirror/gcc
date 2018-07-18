/* { dg-do compile }  */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_ok }  */
/* { dg-options "-O2 -ffast-math" }  */
/* { dg-add-options arm_v8_2a_fp16_neon }  */

/* Test compiler use of FP16 FMA/FMS instructions with -ffast-math.  */

#include <arm_neon.h>

float16x4_t
test_vfma_1 (float16x4_t a, float16x4_t b, float16x4_t c)
{
  return vadd_f16 (vmul_f16 (a, b), c);
}

float16x4_t
test_vfma_2 (float16x4_t a, float16x4_t b, float16x4_t c)
{
  return vsub_f16 (vmul_f16 (a, b), vneg_f16 (c));
}

float16x4_t
test_vfma_3 (float16x4_t a, float16x4_t b, float16x4_t c)
{
  return vsub_f16 (vmul_f16 (vneg_f16 (a), vneg_f16 (b)), vneg_f16 (c));
}

float16x4_t
test_vfma_4 (float16x4_t a, float16x4_t b, float16x4_t c)
{
  return vsub_f16 (vmul_f16 (a, b), vneg_f16 (c));
}
/* { dg-final { scan-assembler-times {vfma\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 4 } }  */

float16x8_t
test_vfmaq_1 (float16x8_t a, float16x8_t b, float16x8_t c)
{
  return vaddq_f16 (vmulq_f16 (a, b), c);
}

float16x8_t
test_vfmaq_2 (float16x8_t a, float16x8_t b, float16x8_t c)
{
  return vsubq_f16 (vmulq_f16 (a, b), vnegq_f16 (c));
}

float16x8_t
test_vfmaq_3 (float16x8_t a, float16x8_t b, float16x8_t c)
{
  return vsubq_f16 (vmulq_f16 (vnegq_f16 (a), vnegq_f16 (b)), vnegq_f16 (c));
}

float16x8_t
test_vfmaq_4 (float16x8_t a, float16x8_t b, float16x8_t c)
{
  return vsubq_f16 (vmulq_f16 (a, b), vnegq_f16 (c));
}
/* { dg-final { scan-assembler-times {vfma\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 4 } }  */

float16x4_t
test_vfms_1 (float16x4_t a, float16x4_t b, float16x4_t c)
{
  return vsub_f16 (c, vmul_f16 (a, b));
}

float16x4_t
test_vfms_2 (float16x4_t a, float16x4_t b, float16x4_t c)
{
  return vsub_f16 (a, vmul_f16 (b, c));
}

float16x4_t
test_vfms_3 (float16x4_t a, float16x4_t b, float16x4_t c)
{
  return vadd_f16 (vmul_f16 (vneg_f16 (a), b), c);
}

float16x4_t
test_vfms_4 (float16x4_t a, float16x4_t b, float16x4_t c)
{
  return vadd_f16 (vmul_f16 (a, vneg_f16 (b)), c);
}
/* { dg-final { scan-assembler-times {vfms\.f16\td[0-9]+, d[0-9]+, d[0-9]+} 4 } } */

float16x8_t
test_vfmsq_1 (float16x8_t a, float16x8_t b, float16x8_t c)
{
  return vsubq_f16 (c, vmulq_f16 (a, b));
}

float16x8_t
test_vfmsq_2 (float16x8_t a, float16x8_t b, float16x8_t c)
{
  return vsubq_f16 (a, vmulq_f16 (b, c));
}

float16x8_t
test_vfmsq_3 (float16x8_t a, float16x8_t b, float16x8_t c)
{
  return vaddq_f16 (vmulq_f16 (vnegq_f16 (a), b), c);
}

float16x8_t
test_vfmsq_4 (float16x8_t a, float16x8_t b, float16x8_t c)
{
  return vaddq_f16 (vmulq_f16 (a, vnegq_f16 (b)), c);
}
/* { dg-final { scan-assembler-times {vfms\.f16\tq[0-9]+, q[0-9]+, q[0-9]+} 4 } } */
