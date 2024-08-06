/* { dg-do compile } */
/* { dg-additional-options "-O3 -march=armv9-a+faminmax" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon.h"

/*
** test_vamax_f16:
**	famax	v0.4h, v0.4h, v1.4h
**	ret
*/
float16x4_t
test_vamax_f16 (float16x4_t a, float16x4_t b)
{
  return vamax_f16 (a, b);
}

/*
** test_vamaxq_f16:
**	famax	v0.8h, v0.8h, v1.8h
**	ret
*/
float16x8_t
test_vamaxq_f16 (float16x8_t a, float16x8_t b)
{
  return vamaxq_f16 (a, b);
}

/*
** test_vamax_f32:
**	famax	v0.2s, v0.2s, v1.2s
**	ret
*/
float32x2_t
test_vamax_f32 (float32x2_t a, float32x2_t b)
{
  return vamax_f32 (a, b);
}

/*
** test_vamaxq_f32:
**	famax	v0.4s, v0.4s, v1.4s
**	ret
*/
float32x4_t
test_vamaxq_f32 (float32x4_t a, float32x4_t b)
{
  return vamaxq_f32 (a, b);
}

/*
** test_vamaxq_f64:
**	famax	v0.2d, v0.2d, v1.2d
**	ret
*/
float64x2_t
test_vamaxq_f64 (float64x2_t a, float64x2_t b)
{
  return vamaxq_f64 (a, b);
}

/*
** test_vamin_f16:
**	famin	v0.4h, v0.4h, v1.4h
**	ret
*/
float16x4_t
test_vamin_f16 (float16x4_t a, float16x4_t b)
{
  return vamin_f16 (a, b);
}

/*
** test_vaminq_f16:
**	famin	v0.8h, v0.8h, v1.8h
**	ret
*/
float16x8_t
test_vaminq_f16 (float16x8_t a, float16x8_t b)
{
  return vaminq_f16 (a, b);
}

/*
** test_vamin_f32:
**	famin	v0.2s, v0.2s, v1.2s
**	ret
*/
float32x2_t
test_vamin_f32 (float32x2_t a, float32x2_t b)
{
  return vamin_f32 (a, b);
}

/*
** test_vaminq_f32:
**	famin	v0.4s, v0.4s, v1.4s
**	ret
*/
float32x4_t
test_vaminq_f32 (float32x4_t a, float32x4_t b)
{
  return vaminq_f32 (a, b);
}

/*
** test_vaminq_f64:
**	famin	v0.2d, v0.2d, v1.2d
**	ret
*/
float64x2_t
test_vaminq_f64 (float64x2_t a, float64x2_t b)
{
  return vaminq_f64 (a, b);
}
