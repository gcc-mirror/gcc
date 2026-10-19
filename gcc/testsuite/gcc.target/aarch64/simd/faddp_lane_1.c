/* { dg-do run } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

__attribute__ ((noipa)) float32_t
lane0_q (float32x4_t a, float32x4_t b)
{
  return vgetq_lane_f32 (vpaddq_f32 (a, b), 0);
}

__attribute__ ((noipa)) float32_t
lane3_q (float32x4_t a, float32x4_t b)
{
  return vgetq_lane_f32 (vpaddq_f32 (a, b), 3);
}

__attribute__ ((noipa)) float32x2_t
low_q (float32x4_t a, float32x4_t b)
{
  return vget_low_f32 (vpaddq_f32 (a, b));
}

__attribute__ ((noipa)) float32_t
lane1_d (float32x2_t a, float32x2_t b)
{
  return vget_lane_f32 (vpadd_f32 (a, b), 1);
}

__attribute__ ((noipa)) float64_t
lane0_q64 (float64x2_t a, float64x2_t b)
{
  return vgetq_lane_f64 (vpaddq_f64 (a, b), 0);
}

__attribute__ ((noipa)) float32x4_t
const_q (void)
{
  float32_t a[] = { 1, 2, 4, 8 };
  float32_t b[] = { 16, 32, 64, 128 };
  return vpaddq_f32 (vld1q_f32 (a), vld1q_f32 (b));
}

__attribute__ ((noipa)) float32_t
sum_d (float32x2_t a)
{
  return vaddv_f32 (a);
}

int
main (void)
{
  float32_t a32[] = { 1, 2, 4, 8 };
  float32_t b32[] = { 16, 32, 64, 128 };
  float64_t a64[] = { 1, 2 };
  float64_t b64[] = { 4, 8 };
  float32x4_t a = vld1q_f32 (a32);
  float32x4_t b = vld1q_f32 (b32);
  float32_t res[4];

  if (lane0_q (a, b) != 3 || lane3_q (a, b) != 192)
    __builtin_abort ();
  vst1_f32 (res, low_q (a, b));
  if (res[0] != 3 || res[1] != 12)
    __builtin_abort ();
  if (lane1_d (vld1_f32 (a32), vld1_f32 (b32)) != 48)
    __builtin_abort ();
  if (lane0_q64 (vld1q_f64 (a64), vld1q_f64 (b64)) != 3)
    __builtin_abort ();
  vst1q_f32 (res, const_q ());
  if (res[0] != 3 || res[1] != 12 || res[2] != 48 || res[3] != 192)
    __builtin_abort ();
  if (sum_d (vld1_f32 (b32)) != 48)
    __builtin_abort ();
  return 0;
}
