/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */

#include <arm_neon.h>

void
test_vst1_bf16_x2 (bfloat16_t *ptr, bfloat16x4x2_t val)
{
  vst1_bf16_x2 (ptr, val);
}

void
test_vst1q_bf16_x2 (bfloat16_t *ptr, bfloat16x8x2_t val)
{
  vst1q_bf16_x2 (ptr, val);
}

void
test_vst1_bf16_x3 (bfloat16_t *ptr, bfloat16x4x3_t val)
{
  vst1_bf16_x3 (ptr, val);
}

void
test_vst1q_bf16_x3 (bfloat16_t *ptr, bfloat16x8x3_t val)
{
  vst1q_bf16_x3 (ptr, val);
}

void
test_vst1_bf16_x4 (bfloat16_t *ptr, bfloat16x4x4_t val)
{
  vst1_bf16_x4 (ptr, val);
}

void
test_vst1q_bf16_x4 (bfloat16_t *ptr, bfloat16x8x4_t val)
{
  vst1q_bf16_x4 (ptr, val);
}

void
test_vst1_lane_bf16 (bfloat16_t *ptr, bfloat16x4_t val)
{
  vst1_lane_bf16 (ptr, val, 3);
}

void
test_vst1q_lane_bf16 (bfloat16_t *ptr, bfloat16x8_t val)
{
  vst1q_lane_bf16 (ptr, val, 7);
}

void
test_vst1_bf16 (bfloat16_t *ptr, bfloat16x4_t val)
{
  vst1_bf16 (ptr, val);
}

void
test_vst1q_bf16 (bfloat16_t *ptr, bfloat16x8_t val)
{
  vst1q_bf16 (ptr, val);
}

void
test_vst2_bf16 (bfloat16_t *ptr, bfloat16x4x2_t val)
{
  vst2_bf16 (ptr, val);
}

void
test_vst2q_bf16 (bfloat16_t *ptr, bfloat16x8x2_t val)
{
  vst2q_bf16 (ptr, val);
}

void
test_vst3_bf16 (bfloat16_t *ptr, bfloat16x4x3_t val)
{
  vst3_bf16 (ptr, val);
}

void
test_vst3q_bf16 (bfloat16_t *ptr, bfloat16x8x3_t val)
{
  vst3q_bf16 (ptr, val);
}

void
test_vst4_bf16 (bfloat16_t *ptr, bfloat16x4x4_t val)
{
  vst4_bf16 (ptr, val);
}

void
test_vst4q_bf16 (bfloat16_t *ptr, bfloat16x8x4_t val)
{
  vst4q_bf16 (ptr, val);
}

int main()
{
  return 0;
}
