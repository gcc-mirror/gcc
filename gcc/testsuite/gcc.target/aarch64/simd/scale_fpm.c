/* { dg-do compile } */
/* { dg-additional-options "-O3 -march=armv9-a+fp8" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon.h"

/*
** test_vscale_f16:
**	fscale	v0.4h, v0.4h, v1.4h
**	ret
*/
float16x4_t
test_vscale_f16 (float16x4_t a, int16x4_t b)
{
  return vscale_f16 (a, b);
}

/*
** test_vscaleq_f16:
**	fscale	v0.8h, v0.8h, v1.8h
**	ret
*/
float16x8_t
test_vscaleq_f16 (float16x8_t a, int16x8_t b)
{
  return vscaleq_f16 (a, b);
}

/*
** test_vscale_f32:
**	fscale	v0.2s, v0.2s, v1.2s
**	ret
*/
float32x2_t
test_vscale_f32 (float32x2_t a, int32x2_t b)
{
  return vscale_f32 (a, b);
}

/*
** test_vscaleq_f32:
**	fscale	v0.4s, v0.4s, v1.4s
**	ret
*/
float32x4_t
test_vscaleq_f32 (float32x4_t a, int32x4_t b)
{
  return vscaleq_f32 (a, b);
}

/*
** test_vscaleq_f64:
**	fscale	v0.2d, v0.2d, v1.2d
**	ret
*/
float64x2_t
test_vscaleq_f64 (float64x2_t a, int64x2_t b)
{
  return vscaleq_f64 (a, b);
}
