/* { dg-do compile } */
/* { dg-additional-options "-O3 -march=armv9-a+fp8dot2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon.h"

/*
** test_vdot_f16_fpm:
**	msr	fpmr, x0
**	fdot	v0.4h, v1.8b, v2.8b
**	ret
*/
float16x4_t
test_vdot_f16_fpm (float16x4_t a, mfloat8x8_t b, mfloat8x8_t c, fpm_t d)
{
  return vdot_f16_mf8_fpm (a, b, c, d);
}

/*
** test_vdotq_f16_fpm:
**	msr	fpmr, x0
**	fdot	v0.8h, v1.16b, v2.16b
**	ret
*/
float16x8_t
test_vdotq_f16_fpm (float16x8_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vdotq_f16_mf8_fpm (a, b, c, d);
}

/*
** test_vdot_lane_f16_fpm_0:
**	msr	fpmr, x0
**	fdot	v0.4h, v1.8b, v2.2b\[0\]
**	ret
*/
float16x4_t
test_vdot_lane_f16_fpm_0 (float16x4_t a, mfloat8x8_t b, mfloat8x8_t c, fpm_t d)
{
  return vdot_lane_f16_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vdot_lane_f16_fpm_3:
**	msr	fpmr, x0
**	fdot	v0.4h, v1.8b, v2.2b\[3\]
**	ret
*/
float16x4_t
test_vdot_lane_f16_fpm_3 (float16x4_t a, mfloat8x8_t b, mfloat8x8_t c, fpm_t d)
{
  return vdot_lane_f16_mf8_fpm (a, b, c, 3, d);
}

/*
** test_vdot_laneq_f16_fpm_0:
**	msr	fpmr, x0
**	fdot	v0.4h, v1.8b, v2.2b\[0\]
**	ret
*/
float16x4_t
test_vdot_laneq_f16_fpm_0 (float16x4_t a, mfloat8x8_t b, mfloat8x16_t c, fpm_t d)
{
  return vdot_laneq_f16_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vdot_laneq_f16_fpm_7:
**	msr	fpmr, x0
**	fdot	v0.4h, v1.8b, v2.2b\[7\]
**	ret
*/
float16x4_t
test_vdot_laneq_f16_fpm_7 (float16x4_t a, mfloat8x8_t b, mfloat8x16_t c, fpm_t d)
{
  return vdot_laneq_f16_mf8_fpm (a, b, c, 7, d);
}

/*
** test_vdotq_lane_f16_fpm_0:
**	msr	fpmr, x0
**	fdot	v0.8h, v1.16b, v2.2b\[0\]
**	ret
*/
float16x8_t
test_vdotq_lane_f16_fpm_0 (float16x8_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vdotq_lane_f16_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vdotq_lane_f16_fpm_3:
**	msr	fpmr, x0
**	fdot	v0.8h, v1.16b, v2.2b\[3\]
**	ret
*/
float16x8_t
test_vdotq_lane_f16_fpm_3 (float16x8_t a, mfloat8x16_t b, mfloat8x8_t c, fpm_t d)
{
  return vdotq_lane_f16_mf8_fpm (a, b, c, 3, d);
}

/*
** test_vdotq_laneq_f16_fpm_0:
**	msr	fpmr, x0
**	fdot	v0.8h, v1.16b, v2.2b\[0\]
**	ret
*/
float16x8_t
test_vdotq_laneq_f16_fpm_0 (float16x8_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vdotq_laneq_f16_mf8_fpm (a, b, c, 0, d);
}

/*
** test_vdotq_laneq_f16_fpm_7:
**	msr	fpmr, x0
**	fdot	v0.8h, v1.16b, v2.2b\[7\]
**	ret
*/
float16x8_t
test_vdotq_laneq_f16_fpm_7 (float16x8_t a, mfloat8x16_t b, mfloat8x16_t c, fpm_t d)
{
  return vdotq_laneq_f16_mf8_fpm (a, b, c, 7, d);
}
