/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

/* The FADDP inputs are subregs of vectors with a different element mode.  */

/*
** sum_f32:
**	faddp	s0, v0\.2s
**	ret
*/
float32_t
sum_f32 (int32x2_t x)
{
  return vaddv_f32 (vreinterpret_f32_s32 (x));
}

/*
** sum_f64:
**	faddp	d0, v0\.2d
**	ret
*/
float64_t
sum_f64 (int64x2_t x)
{
  return vaddvq_f64 (vreinterpretq_f64_s64 (x));
}

/*
** pair_f32:
**	faddp	v0\.2s, v0\.2s, v1\.2s
**	ret
*/
float32x2_t
pair_f32 (int32x2_t x, int32x2_t y)
{
  return vpadd_f32 (vreinterpret_f32_s32 (x), vreinterpret_f32_s32 (y));
}

/*
** pair_f64:
**	faddp	v0\.2d, v0\.2d, v1\.2d
**	ret
*/
float64x2_t
pair_f64 (int64x2_t x, int64x2_t y)
{
  return vpaddq_f64 (vreinterpretq_f64_s64 (x), vreinterpretq_f64_s64 (y));
}
