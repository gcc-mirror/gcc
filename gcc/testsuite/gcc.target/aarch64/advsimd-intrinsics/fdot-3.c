/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-require-effective-target aarch64_f16f32dot_neon_ok } */
/* { dg-add-options aarch64_f16f32dot_neon }  */
/* { dg-additional-options "--save-temps" } */

#include <arm_neon.h>

float32x2_t ufoo_lane(float32x2_t r, float16x4_t x, float16x4_t y)
{
  return vdot_lane_f32_f16 (r, x, y, 2);
}

float32x2_t ufoo_laneq(float32x2_t r, float16x4_t x, float16x8_t y)
{
  return vdot_laneq_f32_f16 (r, x, y, 4);
}

float32x4_t ufooq_lane(float32x4_t r, float16x8_t x, float16x4_t y)
{
  return vdotq_lane_f32_f16 (r, x, y, 3);
}

float32x4_t ufooq_laneq(float32x4_t r, float16x8_t x, float16x8_t y)
{
  return vdotq_laneq_f32_f16 (r, x, y, 5);
}

/* { dg-error {lane 2 out of range 0 - 1} "" { target *-*-* } 0 } */
/* { dg-error {lane 4 out of range 0 - 3} "" { target *-*-* } 0 } */
/* { dg-error {lane 3 out of range 0 - 1} "" { target *-*-* } 0 } */
/* { dg-error {lane 5 out of range 0 - 3} "" { target *-*-* } 0 } */