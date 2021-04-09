/* { dg-do assemble { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */
/* { dg-additional-options "-save-temps -march=armv8.2-a+bf16+fp16" } */

#include "arm_neon.h"

float32x2_t
test_vbfdot_vcreate (float32x2_t r, uint64_t a, uint64_t b)
{
  bfloat16x4_t _a = vcreate_bf16(a);
  bfloat16x4_t _b = vcreate_bf16(b);

  return vbfdot_f32 (r, _a, _b);
}
/* { dg-final { scan-assembler {vdot.bf16\td[0-9]+, d[0-9]+, d[0-9]+} } } */

bfloat16x8_t test_vcombine_bf16 (bfloat16x4_t a, bfloat16x4_t b)
{
  return vcombine_bf16 (a, b);
}

bfloat16x4_t test_vget_high_bf16 (bfloat16x8_t a)
{
  return vget_high_bf16 (a);
}

bfloat16x4_t test_vget_low_bf16 (bfloat16x8_t a)
{
  return vget_low_bf16 (a);
}

bfloat16_t test_vget_lane_bf16 (bfloat16x4_t a)
{
  return vget_lane_bf16 (a, 1);
}

bfloat16_t test_vgetq_lane_bf16 (bfloat16x8_t a)
{
  return vgetq_lane_bf16 (a, 7);
}

bfloat16x4_t test_vset_lane_bf16 (bfloat16_t a, bfloat16x4_t b)
{
  return vset_lane_bf16 (a, b, 1);
}

bfloat16x8_t test_vsetq_lane_bf16 (bfloat16_t a, bfloat16x8_t b)
{
  return vsetq_lane_bf16 (a, b, 7);
}

bfloat16x4_t vdup_test (bfloat16_t a)
{
  return vdup_n_bf16 (a);
}
/* { dg-final { scan-assembler {vdup\.16\td[0-9]+, r[0-9]+} } }  */

bfloat16x8_t vdupq_test (bfloat16_t a)
{
  return vdupq_n_bf16 (a);
}
/* { dg-final { scan-assembler {vdup\.16\tq[0-9]+, r[0-9]+} } }  */


bfloat16x4_t test_vdup_lane_bf16 (bfloat16x4_t a)
{
  return vdup_lane_bf16 (a, 1);
}
/* { dg-final { scan-assembler-times {vdup\.16\td[0-9]+, d[0-9]+\[1\]} 1 } }  */

bfloat16x8_t test_vdupq_lane_bf16 (bfloat16x4_t a)
{
  return vdupq_lane_bf16 (a, 1);
}
/* { dg-final { scan-assembler-times {vdup\.16\tq[0-9]+, d[0-9]+\[1\]} 1 } }  */

bfloat16x4_t test_vdup_laneq_bf16 (bfloat16x8_t a)
{
  return vdup_laneq_bf16 (a, 3);
}

bfloat16x8_t test_vdupq_laneq_bf16 (bfloat16x8_t a)
{
  return vdupq_laneq_bf16 (a, 3);
}

bfloat16_t test_vduph_lane_bf16 (bfloat16x4_t a)
{
  return vduph_lane_bf16 (a, 1);
}

bfloat16_t test_vduph_laneq_bf16 (bfloat16x8_t a)
{
  return vduph_laneq_bf16 (a, 7);
}
