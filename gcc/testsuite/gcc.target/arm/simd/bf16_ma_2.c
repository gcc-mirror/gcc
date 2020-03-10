/* { dg-do compile { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */

#include "arm_neon.h"

/* Test lane index limits for vfmabq_lane_f32  */
float32x4_t
test_vfmabq_lane_f32_low (float32x4_t r, bfloat16x8_t a, bfloat16x4_t b)
{
  /* { dg-error "lane -1 out of range 0 - 3" "" { target *-*-* } 0 } */
  return vbfmlalbq_lane_f32 (r, a, b, -1);
}

float32x4_t
test_vfmabq_lane_f32_high (float32x4_t r, bfloat16x8_t a, bfloat16x4_t b)
{
  /* { dg-error "lane 4 out of range 0 - 3" "" { target *-*-* } 0 } */
  return vbfmlalbq_lane_f32 (r, a, b, 4);
}

/* Test lane index limits for vfmatq_lane_f32  */
float32x4_t
test_vfmatq_lane_f32_low (float32x4_t r, bfloat16x8_t a, bfloat16x4_t b)
{
  /* { dg-error "lane -2 out of range 0 - 3" "" { target *-*-* } 0 } */
  return vbfmlaltq_lane_f32 (r, a, b, -2);
}

float32x4_t
test_vfmatq_lane_f32_high (float32x4_t r, bfloat16x8_t a, bfloat16x4_t b)
{
  /* { dg-error "lane 5 out of range 0 - 3" "" { target *-*-* } 0 } */
  return vbfmlaltq_lane_f32 (r, a, b, 5);
}
