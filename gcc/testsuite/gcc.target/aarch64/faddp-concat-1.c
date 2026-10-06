/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

/*
** pair_f32:	{ target aarch64_little_endian }
**	faddp\tv0\.2s, v0\.2s, v1\.2s
**	ret
*/
/*
** pair_f32:	{ target aarch64_big_endian }
**	faddp\tv0\.2s, v1\.2s, v0\.2s
**	ret
*/
__attribute__ ((noipa)) float32x2_t
pair_f32 (float32x2_t a, float32x2_t b)
{
  return (float32x2_t) { vaddv_f32 (a), vaddv_f32 (b) };
}

/*
** pair_f64:	{ target aarch64_little_endian }
**	faddp\tv0\.2d, v0\.2d, v1\.2d
**	ret
*/
/*
** pair_f64:	{ target aarch64_big_endian }
**	faddp\tv0\.2d, v1\.2d, v0\.2d
**	ret
*/
__attribute__ ((noipa)) float64x2_t
pair_f64 (float64x2_t a, float64x2_t b)
{
  return (float64x2_t) { vaddvq_f64 (a), vaddvq_f64 (b) };
}

/*
** pair_mixed:	{ target aarch64_little_endian }
**	faddp\tv0\.2s, v0\.2s, v1\.2s
**	ret
*/
/*
** pair_mixed:	{ target aarch64_big_endian }
**	faddp\tv0\.2s, v1\.2s, v0\.2s
**	ret
*/
__attribute__ ((noipa)) float32x2_t
pair_mixed (float32x2_t a, float32x2_t b)
{
  return (float32x2_t) { vaddv_f32 (a), b[0] + b[1] };
}

/*
** sum_lanes:
**	faddp\ts0, v0\.2s
**	ret
*/
__attribute__ ((noipa)) float32_t
sum_lanes (float32x2_t a)
{
  return a[0] + a[1];
}
