/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target aarch64_f16f32dot_neon_ok } */
/* { dg-add-options aarch64_f16f32dot_neon }  */
/* { dg-additional-options "-save-temps" } */
/* { dg-final { check-function-bodies "**" "" {-O[^0]} } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

#include <arm_neon.h>

/*
**ufoo:
**	fdot	v0.2s, (v1.4h, v2.4h|v2.4h, v1.4h)
**	ret
*/
float32x2_t ufoo(float32x2_t r, float16x4_t x, float16x4_t y)
{
  return vdot_f32_f16 (r, x, y);
}

/*
**ufooq:
**	fdot	v0.4s, (v1.8h, v2.8h|v2.8h, v1.8h)
**	ret
*/
float32x4_t ufooq(float32x4_t r, float16x8_t x, float16x8_t y)
{
  return vdotq_f32_f16 (r, x, y);
}

/*
**ufoo_lane:
**	fdot	v0.2s, v1.4h, v2.2h\[0\]
**	ret
*/
float32x2_t ufoo_lane(float32x2_t r, float16x4_t x, float16x4_t y)
{
  return vdot_lane_f32_f16 (r, x, y, 0);
}

/*
**ufoo_laneq:
**	fdot	v0.2s, v1.4h, v2.2h\[2\]
**	ret
*/
float32x2_t ufoo_laneq(float32x2_t r, float16x4_t x, float16x8_t y)
{
  return vdot_laneq_f32_f16 (r, x, y, 2);
}

/*
**ufooq_lane:
**	fdot	v0.4s, v1.8h, v2.2h\[1\]
**	ret
*/
float32x4_t ufooq_lane(float32x4_t r, float16x8_t x, float16x4_t y)
{
  return vdotq_lane_f32_f16 (r, x, y, 1);
}

/*
**ufooq_laneq:
**	fdot	v0.4s, v1.8h, v2.2h\[2\]
**	ret
*/
float32x4_t ufooq_laneq(float32x4_t r, float16x8_t x, float16x8_t y)
{
  return vdotq_laneq_f32_f16 (r, x, y, 2);
}