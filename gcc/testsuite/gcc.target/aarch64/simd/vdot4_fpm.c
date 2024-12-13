/* { dg-do compile } */
/* { dg-additional-options "-O3 -march=armv9-a+fp8dot4" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon.h"

/*
** test_vdot_f32_fpm:
**	msr	fpmr, x0
**	fdot	v0.2s, v1.8b, v2.8b
**	ret
*/
float32x2_t
test_vdot_f32_fpm (float32x2_t a, mfloat8x8_t b, mfloat8x8_t c, fpm_t d)
{
  return vdot_f32_mf8_fpm (a, b, c, d);
}

/*
** test_vdotq_f32_fpm:
**	msr	fpmr, x0
**	fdot	v0.4s, v1.16b, v2.16b
**	ret
*/
float32x4_t
test_vdotq_f32_fpm (float32x4_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vdotq_f32_mf8_fpm (a, b, c, d);
}

/*
** test_vdot_lane_f32_fpm_0:
**	msr	fpmr, x0
**	fdot	v0.2s, v1.8b, v2.4b\[0\]
**	ret
*/
float32x2_t
test_vdot_lane_f32_fpm_0 (float32x2_t a, mfloat8x8_t b, mfloat8x8_t c, fpm_t d)
{
  return vdot_lane_f32_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vdot_lane_f32_fpm_1:
**	msr	fpmr, x0
**	fdot	v0.2s, v1.8b, v2.4b\[1\]
**	ret
*/
float32x2_t
test_vdot_lane_f32_fpm_1 (float32x2_t a, mfloat8x8_t b, mfloat8x8_t c, fpm_t d)
{
  return vdot_lane_f32_mf8_fpm (a, b, c, 1, d);
}

/*
** test_vdot_laneq_f32_fpm_0:
**	msr	fpmr, x0
**	fdot	v0.2s, v1.8b, v2.4b\[0\]
**	ret
*/
float32x2_t
test_vdot_laneq_f32_fpm_0 (float32x2_t a, mfloat8x8_t b, mfloat8x16_t c, fpm_t d)
{
  return vdot_laneq_f32_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vdot_laneq_f32_fpm_3:
**	msr	fpmr, x0
**	fdot	v0.2s, v1.8b, v2.4b\[3\]
**	ret
*/
float32x2_t
test_vdot_laneq_f32_fpm_3 (float32x2_t a, mfloat8x8_t b, mfloat8x16_t c, fpm_t d)
{
  return vdot_laneq_f32_mf8_fpm (a, b, c, 3, d);
}

/*
** test_vdotq_lane_f32_fpm_0:
**	msr	fpmr, x0
**	fdot	v0.4s, v1.16b, v2.4b\[0\]
**	ret
*/
float32x4_t
test_vdotq_lane_f32_fpm_0 (float32x4_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vdotq_lane_f32_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vdotq_lane_f32_fpm_1:
**	msr	fpmr, x0
**	fdot	v0.4s, v1.16b, v2.4b\[1\]
**	ret
*/
float32x4_t
test_vdotq_lane_f32_fpm_1 (float32x4_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vdotq_lane_f32_mf8_fpm (a, b, c, 1, d);
}

/*
** test_vdotq_laneq_f32_fpm_0:
**	msr	fpmr, x0
**	fdot	v0.4s, v1.16b, v2.4b\[0\]
**	ret
*/
float32x4_t
test_vdotq_laneq_f32_fpm_0 (float32x4_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vdotq_laneq_f32_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vdotq_laneq_f32_fpm_3:
**	msr	fpmr, x0
**	fdot	v0.4s, v1.16b, v2.4b\[3\]
**	ret
*/
float32x4_t
test_vdotq_laneq_f32_fpm_3 (float32x4_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vdotq_laneq_f32_mf8_fpm (a, b, c, 3, d);
}
