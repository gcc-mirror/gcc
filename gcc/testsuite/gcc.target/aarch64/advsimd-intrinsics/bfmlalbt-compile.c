/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include <arm_neon.h>

/*
**test_bfmlalb:
**      bfmlalb	v0.4s, v1.8h, v2.8h
**      ret
*/
float32x4_t test_bfmlalb (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b)
{
  return vbfmlalbq_f32 (r, a, b);
}

/*
**test_bfmlalt:
**      bfmlalt	v0.4s, v1.8h, v2.8h
**      ret
*/
float32x4_t test_bfmlalt (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b)
{
  return vbfmlaltq_f32 (r, a, b);
}

/*
**test_bfmlalb_lane:
**      bfmlalb	v0.4s, v1.8h, v2.h[0]
**      ret
*/
float32x4_t test_bfmlalb_lane (float32x4_t r, bfloat16x8_t a, bfloat16x4_t b)
{
  return vbfmlalbq_lane_f32 (r, a, b, 0);
}

/*
**test_bfmlalt_lane:
**      bfmlalt	v0.4s, v1.8h, v2.h[2]
**      ret
*/
float32x4_t test_bfmlalt_lane (float32x4_t r, bfloat16x8_t a, bfloat16x4_t b)
{
  return vbfmlaltq_lane_f32 (r, a, b, 2);
}

/*
**test_bfmlalb_laneq:
**      bfmlalb	v0.4s, v1.8h, v2.h[4]
**      ret
*/
float32x4_t test_bfmlalb_laneq (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b)
{
  return vbfmlalbq_laneq_f32 (r, a, b, 4);
}

/*
**test_bfmlalt_laneq:
**      bfmlalt	v0.4s, v1.8h, v2.h[7]
**      ret
*/
float32x4_t test_bfmlalt_laneq (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b)
{
  return vbfmlaltq_laneq_f32 (r, a, b, 7);
}
