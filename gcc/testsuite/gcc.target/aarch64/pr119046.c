/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

#include <arm_neon.h>

float32x4_t madd_helper_1(float32x4_t a, float32x4_t b, float32x4_t d)
{
  float32x4_t t = a;
  t = vfmaq_f32 (t, vdupq_n_f32(vgetq_lane_f32 (b, 1)), d);
  t = vfmaq_f32 (t, vdupq_n_f32(vgetq_lane_f32 (b, 1)), d);
  return t;
}

/* { dg-final { scan-assembler-not {\tdup\tv[0-9]+\.4s, v[0-9]+.s\[1\]\n} } } */
/* { dg-final { scan-assembler-times {\tfmla\tv[0-9]+\.4s, v[0-9]+\.4s, v[0-9]+\.s\[1\]\n} 2 } } */

