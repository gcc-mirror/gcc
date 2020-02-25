/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */

#include "arm_neon.h"

float32x4_t
test_vbfdotq_lane_f32_a (float32x4_t r, bfloat16x8_t a, bfloat16x4_t b)
{
  /* { dg-error "lane -1 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vbfdotq_lane_f32 (r, a, b, -1);
}

float32x4_t
test_vbfdotq_lane_f32_b (float32x4_t r, bfloat16x8_t a, bfloat16x4_t b)
{
  /* { dg-error "lane 2 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vbfdotq_lane_f32 (r, a, b, 2);
}

float32x4_t
test_vbfdotq_laneq_f32_a (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b)
{
  /* { dg-error "lane -1 out of range 0 - 3" "" { target *-*-* } 0 } */
  return vbfdotq_laneq_f32 (r, a, b, -1);
}

float32x4_t
test_vbfdotq_laneq_f32_b (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b)
{
  /* { dg-error "lane 4 out of range 0 - 3" "" { target *-*-* } 0 } */
  return vbfdotq_laneq_f32 (r, a, b, 4);
}
