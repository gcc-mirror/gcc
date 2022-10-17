/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" "" {-O[^0]} } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

#include <arm_neon.h>

/*
**ufoo:
**	bfdot	v0.2s, (v1.4h, v2.4h|v2.4h, v1.4h)
**	ret
*/
float32x2_t ufoo(float32x2_t r, bfloat16x4_t x, bfloat16x4_t y)
{
  return vbfdot_f32 (r, x, y);
}

/*
**ufooq:
**	bfdot	v0.4s, (v1.8h, v2.8h|v2.8h, v1.8h)
**	ret
*/
float32x4_t ufooq(float32x4_t r, bfloat16x8_t x, bfloat16x8_t y)
{
  return vbfdotq_f32 (r, x, y);
}

/*
**ufoo_lane:
**	bfdot	v0.2s, v1.4h, v2.2h\[0\]
**	ret
*/
float32x2_t ufoo_lane(float32x2_t r, bfloat16x4_t x, bfloat16x4_t y)
{
  return vbfdot_lane_f32 (r, x, y, 0);
}

/*
**ufooq_laneq:
**	bfdot	v0.4s, v1.8h, v2.2h\[2\]
**	ret
*/
float32x4_t ufooq_laneq(float32x4_t r, bfloat16x8_t x, bfloat16x8_t y)
{
  return vbfdotq_laneq_f32 (r, x, y, 2);
}

/*
**ufoo_laneq:
**	bfdot	v0.2s, v1.4h, v2.2h\[3\]
**	ret
*/
float32x2_t ufoo_laneq(float32x2_t r, bfloat16x4_t x, bfloat16x8_t y)
{
  return vbfdot_laneq_f32 (r, x, y, 3);
}

/*
**ufooq_lane:
**	bfdot	v0.4s, v1.8h, v2.2h\[1\]
**	ret
*/
float32x4_t ufooq_lane(float32x4_t r, bfloat16x8_t x, bfloat16x4_t y)
{
  return vbfdotq_lane_f32 (r, x, y, 1);
}

/*
**ufoo_untied:
**	mov	v0.8b, v1.8b
**	bfdot	v0.2s, (v2.4h, v3.4h|v3.4h, v2.4h)
**	ret
*/
float32x2_t ufoo_untied(float32x4_t unused, float32x2_t r, bfloat16x4_t x, bfloat16x4_t y)
{
  return vbfdot_f32 (r, x, y);
}

/*
**ufooq_lane_untied:
**	mov	v0.16b, v1.16b
**	bfdot	v0.4s, v2.8h, v3.2h\[1\]
**	ret
*/
float32x4_t ufooq_lane_untied(float32x4_t unused, float32x4_t r, bfloat16x8_t x, bfloat16x4_t y)
{
  return vbfdotq_lane_f32 (r, x, y, 1);
}

