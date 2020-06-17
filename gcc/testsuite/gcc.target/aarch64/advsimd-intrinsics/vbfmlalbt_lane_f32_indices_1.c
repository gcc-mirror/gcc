/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */

#include <arm_neon.h>

void
f_vbfmlaltq_lane_f32 (float32x4_t r, bfloat16x8_t a, bfloat16x4_t b)
{
  /* { dg-error "lane -1 out of range 0 - 3" "" { target *-*-* } 0 } */
  vbfmlaltq_lane_f32 (r, a, b, -1);
  /* { dg-error "lane 4 out of range 0 - 3" "" { target *-*-* } 0 } */
  vbfmlaltq_lane_f32 (r, a, b, 4);
  return;
}

void
f_vbfmlaltq_laneq_f32 (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b)
{
  /* { dg-error "lane -1 out of range 0 - 7" "" { target *-*-* } 0 } */
  vbfmlaltq_laneq_f32 (r, a, b, -1);
  /* { dg-error "lane 8 out of range 0 - 7" "" { target *-*-* } 0 } */
  vbfmlaltq_laneq_f32 (r, a, b, 8);
  return;
}

void
f_vbfmlalbq_lane_f32 (float32x4_t r, bfloat16x8_t a, bfloat16x4_t b)
{
  /* { dg-error "lane -2 out of range 0 - 3" "" { target *-*-* } 0 } */
  vbfmlalbq_lane_f32 (r, a, b, -2);
  /* { dg-error "lane 5 out of range 0 - 3" "" { target *-*-* } 0 } */
  vbfmlalbq_lane_f32 (r, a, b, 5);
  return;
}

void
f_vbfmlalbq_laneq_f32 (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b)
{
  /* { dg-error "lane -2 out of range 0 - 7" "" { target *-*-* } 0 } */
  vbfmlalbq_laneq_f32 (r, a, b, -2);
  /* { dg-error "lane 9 out of range 0 - 7" "" { target *-*-* } 0 } */
  vbfmlalbq_laneq_f32 (r, a, b, 9);
  return;
}
