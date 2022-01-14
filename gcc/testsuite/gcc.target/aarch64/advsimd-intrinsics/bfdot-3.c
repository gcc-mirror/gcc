/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */
/* { dg-additional-options "--save-temps" } */

#include <arm_neon.h>

float32x2_t ufoo_lane(float32x2_t r, bfloat16x4_t x, bfloat16x4_t y)
{
  return vbfdot_lane_f32 (r, x, y, -1); /* { dg-error {lane -1 out of range 0 - 1} "" { target *-*-* } 0 } */
}

float32x4_t ufooq_laneq(float32x4_t r, bfloat16x8_t x, bfloat16x8_t y)
{
  return vbfdotq_laneq_f32 (r, x, y, -1); /* { dg-error {lane -1 out of range 0 - 3} "" { target *-*-* } 0 } */
}

float32x2_t ufoo_laneq(float32x2_t r, bfloat16x4_t x, bfloat16x8_t y)
{
  return vbfdot_laneq_f32 (r, x, y, 4); /* { dg-error {lane 4 out of range 0 - 3} "" { target *-*-* } 0 } */
}

float32x4_t ufooq_lane(float32x4_t r, bfloat16x8_t x, bfloat16x4_t y)
{
  return vbfdotq_lane_f32 (r, x, y, 2); /* { dg-error {lane 2 out of range 0 - 1} "" { target *-*-* } 0 } */
}

