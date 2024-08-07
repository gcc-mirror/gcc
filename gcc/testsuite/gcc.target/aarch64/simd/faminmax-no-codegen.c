/* { dg-do compile } */
/* { dg-additional-options "-O2 -ffast-math -march=armv9-a+faminmax" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon.h"

#pragma GCC target "+nosve"

/*
** test_abs_max_f16:
**	fabs	v1.4h, v1.4h
**	fabs	v0.4h, v0.4h
**	fmax	v0.4h, v0.4h, v1.4h
**	ret
*/
float16x4_t
test_abs_max_f16 (float16x4_t a, float16x4_t b)
{
  return vmax_f16 (vabs_f16 (a), vabs_f16 (b));
}

/*
** test_abs_maxnm_f16:
**	fabs	v1.4h, v1.4h
**	fabs	v0.4h, v0.4h
**	fmaxnm	v0.4h, v0.4h, v1.4h
**	ret
*/
float16x4_t
test_abs_maxnm_f16 (float16x4_t a, float16x4_t b)
{
  return vmaxnm_f16 (vabs_f16 (a), vabs_f16 (b));
}

/*
** test_abs_maxq_f16:
**	fabs	v1.8h, v1.8h
**	fabs	v0.8h, v0.8h
**	fmax	v0.8h, v0.8h, v1.8h
**	ret
*/
float16x8_t
test_abs_maxq_f16 (float16x8_t a, float16x8_t b)
{
  return vmaxq_f16 (vabsq_f16 (a), vabsq_f16 (b));
}

/*
** test_abs_maxnmq_f16:
**	fabs	v1.8h, v1.8h
**	fabs	v0.8h, v0.8h
**	fmaxnm	v0.8h, v0.8h, v1.8h
**	ret
*/
float16x8_t
test_abs_maxnmq_f16 (float16x8_t a, float16x8_t b)
{
  return vmaxnmq_f16 (vabsq_f16 (a), vabsq_f16 (b));
}

/*
** test_abs_max_f32:
**	fabs	v1.2s, v1.2s
**	fabs	v0.2s, v0.2s
**	fmax	v0.2s, v0.2s, v1.2s
**	ret
*/
float32x2_t
test_abs_max_f32 (float32x2_t a, float32x2_t b)
{
  return vmax_f32 (vabs_f32 (a), vabs_f32 (b));
}

/*
** test_abs_maxnm_f32:
**	fabs	v1.2s, v1.2s
**	fabs	v0.2s, v0.2s
**	fmaxnm	v0.2s, v0.2s, v1.2s
**	ret
*/
float32x2_t
test_abs_maxnm_f32 (float32x2_t a, float32x2_t b)
{
  return vmaxnm_f32 (vabs_f32 (a), vabs_f32 (b));
}

/*
** test_abs_maxq_f32:
**	fabs	v1.4s, v1.4s
**	fabs	v0.4s, v0.4s
**	fmax	v0.4s, v0.4s, v1.4s
**	ret
*/
float32x4_t
test_abs_maxq_f32 (float32x4_t a, float32x4_t b)
{
  return vmaxq_f32 (vabsq_f32 (a), vabsq_f32 (b));
}

/*
** test_abs_maxnmq_f32:
**	fabs	v1.4s, v1.4s
**	fabs	v0.4s, v0.4s
**	fmaxnm	v0.4s, v0.4s, v1.4s
**	ret
*/
float32x4_t
test_abs_maxnmq_f32 (float32x4_t a, float32x4_t b)
{
  return vmaxnmq_f32 (vabsq_f32 (a), vabsq_f32 (b));
}

/*
** test_abs_maxq_f64:
**	fabs	v1.2d, v1.2d
**	fabs	v0.2d, v0.2d
**	fmax	v0.2d, v0.2d, v1.2d
**	ret
*/
float64x2_t
test_abs_maxq_f64 (float64x2_t a, float64x2_t b)
{
  return vmaxq_f64 (vabsq_f64 (a), vabsq_f64 (b));
}

/*
** test_abs_maxnmq_f64:
**	fabs	v1.2d, v1.2d
**	fabs	v0.2d, v0.2d
**	fmaxnm	v0.2d, v0.2d, v1.2d
**	ret
*/
float64x2_t
test_abs_maxnmq_f64 (float64x2_t a, float64x2_t b)
{
  return vmaxnmq_f64 (vabsq_f64 (a), vabsq_f64 (b));
}

/*
** test_abs_min_f16:
**	fabs	v1.4h, v1.4h
**	fabs	v0.4h, v0.4h
**	fmin	v0.4h, v0.4h, v1.4h
**	ret
*/
float16x4_t
test_abs_min_f16 (float16x4_t a, float16x4_t b)
{
  return vmin_f16 (vabs_f16 (a), vabs_f16 (b));
}

/*
** test_abs_minnm_f16:
**	fabs	v1.4h, v1.4h
**	fabs	v0.4h, v0.4h
**	fminnm	v0.4h, v0.4h, v1.4h
**	ret
*/
float16x4_t
test_abs_minnm_f16 (float16x4_t a, float16x4_t b)
{
  return vminnm_f16 (vabs_f16 (a), vabs_f16 (b));
}

/*
** test_abs_minq_f16:
**	fabs	v1.8h, v1.8h
**	fabs	v0.8h, v0.8h
**	fmin	v0.8h, v0.8h, v1.8h
**	ret
*/
float16x8_t
test_abs_minq_f16 (float16x8_t a, float16x8_t b)
{
  return vminq_f16 (vabsq_f16 (a), vabsq_f16 (b));
}

/*
** test_abs_minnmq_f16:
**	fabs	v1.8h, v1.8h
**	fabs	v0.8h, v0.8h
**	fminnm	v0.8h, v0.8h, v1.8h
**	ret
*/
float16x8_t
test_abs_minnmq_f16 (float16x8_t a, float16x8_t b)
{
  return vminnmq_f16 (vabsq_f16 (a), vabsq_f16 (b));
}

/*
** test_abs_min_f32:
**	fabs	v1.2s, v1.2s
**	fabs	v0.2s, v0.2s
**	fmin	v0.2s, v0.2s, v1.2s
**	ret
*/
float32x2_t
test_abs_min_f32 (float32x2_t a, float32x2_t b)
{
  return vmin_f32 (vabs_f32 (a), vabs_f32 (b));
}

/*
** test_abs_minnm_f32:
**	fabs	v1.2s, v1.2s
**	fabs	v0.2s, v0.2s
**	fminnm	v0.2s, v0.2s, v1.2s
**	ret
*/
float32x2_t
test_abs_minnm_f32 (float32x2_t a, float32x2_t b)
{
  return vminnm_f32 (vabs_f32 (a), vabs_f32 (b));
}

/*
** test_abs_minq_f32:
**	fabs	v1.4s, v1.4s
**	fabs	v0.4s, v0.4s
**	fmin	v0.4s, v0.4s, v1.4s
**	ret
*/
float32x4_t
test_abs_minq_f32 (float32x4_t a, float32x4_t b)
{
  return vminq_f32 (vabsq_f32 (a), vabsq_f32 (b));
}

/*
** test_abs_minnmq_f32:
**	fabs	v1.4s, v1.4s
**	fabs	v0.4s, v0.4s
**	fminnm	v0.4s, v0.4s, v1.4s
**	ret
*/
float32x4_t
test_abs_minnmq_f32 (float32x4_t a, float32x4_t b)
{
  return vminnmq_f32 (vabsq_f32 (a), vabsq_f32 (b));
}

/*
** test_abs_minq_f64:
**	fabs	v1.2d, v1.2d
**	fabs	v0.2d, v0.2d
**	fmin	v0.2d, v0.2d, v1.2d
**	ret
*/
float64x2_t
test_abs_minq_f64 (float64x2_t a, float64x2_t b)
{
  return vminq_f64 (vabsq_f64 (a), vabsq_f64 (b));
}

/*
** test_abs_minnmq_f64:
**	fabs	v1.2d, v1.2d
**	fabs	v0.2d, v0.2d
**	fminnm	v0.2d, v0.2d, v1.2d
**	ret
*/
float64x2_t
test_abs_minnmq_f64 (float64x2_t a, float64x2_t b)
{
  return vminnmq_f64 (vabsq_f64 (a), vabsq_f64 (b));
}
