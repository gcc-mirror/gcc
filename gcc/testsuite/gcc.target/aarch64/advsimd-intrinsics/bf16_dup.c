/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */
/* { dg-additional-options "-save-temps" } */

#include <arm_neon.h>

float32x2_t test_vcreate (float32x2_t r, uint64_t a, uint64_t b)
{
  bfloat16x4_t _a = vcreate_bf16(a);
  bfloat16x4_t _b = vcreate_bf16(b);

  return vbfdot_f32 (r, _a, _b);
}
/* { dg-final { scan-assembler {bfdot\tv[0-9]+.2s, v[0-9]+.4h, v[0-9]+.4h} } } */

bfloat16x4_t test_vset_lane_bf16 (bfloat16_t a, bfloat16x4_t b)
{
  return vset_lane_bf16 (a, b, 3);
}

bfloat16x8_t test_vsetq_lane_bf16 (bfloat16_t a, bfloat16x8_t b)
{
  return vsetq_lane_bf16 (a, b, 7);
}
/* { dg-final { scan-assembler-times "ins\\t" 2 } } */

bfloat16x4_t vdup_test (bfloat16_t a)
{
  return vdup_n_bf16 (a);
}
/* { dg-final { scan-assembler "dup\\tv\[0-9\]+\.4h, v\[0-9\]+.h\\\[0\\\]" } } */

bfloat16x8_t vdupq_test (bfloat16_t a)
{
  return vdupq_n_bf16 (a);
}

bfloat16x8_t test_vdupq_lane_bf16 (bfloat16x4_t a)
{
  return vdupq_lane_bf16 (a, 1);
}
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.8h, v\[0-9\]+.h\\\[0\\\]" 2 } } */

bfloat16_t test_vget_lane_bf16 (bfloat16x4_t a)
{
  return vget_lane_bf16 (a, 1);
}
/* { dg-final { scan-assembler-times "dup\\th\[0-9\]+, v\[0-9\]+\.h\\\[1\\\]" 2 } } */

bfloat16x4_t test_vdup_lane_bf16 (bfloat16x4_t a)
{
  return vdup_lane_bf16 (a, 1);
}
/* { dg-final { scan-assembler "dup\\tv\[0-9\]+\.4h, v\[0-9\]+\.h\\\[1\\\]" } } */

bfloat16x4_t test_vdup_laneq_bf16 (bfloat16x8_t a)
{
  return vdup_laneq_bf16 (a, 7);
}
/* { dg-final { scan-assembler "dup\\tv\[0-9\]+\.8h, v\[0-9\]+\.h\\\[7\\\]" } } */

bfloat16x8_t test_vdupq_laneq_bf16 (bfloat16x8_t a)
{
  return vdupq_laneq_bf16 (a, 5);
}
/* { dg-final { scan-assembler "dup\\tv\[0-9\]+\.8h, v\[0-9\]+\.h\\\[5\\\]" } } */

bfloat16_t test_vduph_lane_bf16 (bfloat16x4_t a)
{
  return vduph_lane_bf16 (a, 3);
}
/* { dg-final { scan-assembler "dup\\th\[0-9\]+, v\[0-9\]+\.h\\\[3\\\]" } } */

bfloat16_t test_vgetq_lane_bf16 (bfloat16x8_t a)
{
  return vgetq_lane_bf16 (a, 7);
}

bfloat16_t test_vduph_laneq_bf16 (bfloat16x8_t a)
{
  return vduph_laneq_bf16 (a, 7);
}
/* { dg-final { scan-assembler-times "dup\\th\[0-9\]+, v\[0-9\]+\.h\\\[7\\\]" 2 } } */
