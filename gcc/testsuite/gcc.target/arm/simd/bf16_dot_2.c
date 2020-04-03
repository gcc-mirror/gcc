/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */

#include "arm_neon.h"

float32x2_t
test_vbfdot_lane_f32_a (float32x2_t r, bfloat16x4_t a, bfloat16x4_t b)
{
  /* { dg-error "lane -1 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vbfdot_lane_f32 (r, a, b, -1);
}

float32x2_t
test_vbfdot_lane_f32_b (float32x2_t r, bfloat16x4_t a, bfloat16x4_t b)
{
  /* { dg-error "lane 2 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vbfdot_lane_f32 (r, a, b, 2);
}

float32x2_t
test_vbfdot_laneq_f32_a (float32x2_t r, bfloat16x4_t a, bfloat16x8_t b)
{
  /* { dg-error "lane -1 out of range 0 - 3" "" { target *-*-* } 0 } */
  return vbfdot_laneq_f32 (r, a, b, -1);
}

float32x2_t
test_vbfdot_laneq_f32_b (float32x2_t r, bfloat16x4_t a, bfloat16x8_t b)
{
  /* { dg-error "lane 4 out of range 0 - 3" "" { target *-*-* } 0 } */
  return vbfdot_laneq_f32 (r, a, b, 4);
}
