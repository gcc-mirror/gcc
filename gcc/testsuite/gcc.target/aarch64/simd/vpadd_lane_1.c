/* { dg-do run } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

__attribute__ ((noipa)) int32_t
lane0_q (int32x4_t a, int32x4_t b)
{
  return vgetq_lane_s32 (vpaddq_s32 (a, b), 0);
}

__attribute__ ((noipa)) int32_t
lane3_q (int32x4_t a, int32x4_t b)
{
  return vgetq_lane_s32 (vpaddq_s32 (a, b), 3);
}

__attribute__ ((noipa)) int32x2_t
low_q (int32x4_t a, int32x4_t b)
{
  return vget_low_s32 (vpaddq_s32 (a, b));
}

__attribute__ ((noipa)) int32x2_t
high_q (int32x4_t a, int32x4_t b)
{
  return vget_high_s32 (vpaddq_s32 (a, b));
}

__attribute__ ((noipa)) int16_t
lane0_d (int16x4_t a, int16x4_t b)
{
  return vget_lane_s16 (vpadd_s16 (a, b), 0);
}

__attribute__ ((noipa)) int16_t
lane3_d (int16x4_t a, int16x4_t b)
{
  return vget_lane_s16 (vpadd_s16 (a, b), 3);
}

__attribute__ ((noipa)) int64_t
lane1_q64 (int64x2_t a, int64x2_t b)
{
  return vgetq_lane_s64 (vpaddq_s64 (a, b), 1);
}

int
main (void)
{
  int32_t a32[] = { 1, 2, 4, 8 };
  int32_t b32[] = { 16, 32, 64, 128 };
  int16_t a16[] = { 1, 2, 4, 8 };
  int16_t b16[] = { 16, 32, 64, 128 };
  int64_t a64[] = { 1, 2 };
  int64_t b64[] = { 4, 8 };
  int32x4_t a = vld1q_s32 (a32);
  int32x4_t b = vld1q_s32 (b32);
  int32_t res[2];

  if (lane0_q (a, b) != 3 || lane3_q (a, b) != 192)
    __builtin_abort ();
  vst1_s32 (res, low_q (a, b));
  if (res[0] != 3 || res[1] != 12)
    __builtin_abort ();
  vst1_s32 (res, high_q (a, b));
  if (res[0] != 48 || res[1] != 192)
    __builtin_abort ();
  if (lane0_d (vld1_s16 (a16), vld1_s16 (b16)) != 3
      || lane3_d (vld1_s16 (a16), vld1_s16 (b16)) != 192)
    __builtin_abort ();
  if (lane1_q64 (vld1q_s64 (a64), vld1q_s64 (b64)) != 12)
    __builtin_abort ();
  return 0;
}
