/* { dg-do compile } */
/* { dg-require-effective-target aarch64_mbig_endian } */
/* Avoid a dependence on big-endian C library headers.  */
/* { dg-options "-O2 -mbig-endian -ffreestanding" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

/* The architectural lanes of the low half depend only on A.  */

/*
** lane0_q:
**	faddp	v0\.4s, v0\.4s, v0\.4s
**	ret
*/
float32_t
lane0_q (float32x4_t a, float32x4_t b)
{
  return vgetq_lane_f32 (vpaddq_f32 (a, b), 0);
}

/*
** low_q:
**	faddp	v0\.4s, v0\.4s, v0\.4s
**	ret
*/
float32x2_t
low_q (float32x4_t a, float32x4_t b)
{
  return vget_low_f32 (vpaddq_f32 (a, b));
}

/*
** full_d:
**	faddp	v0\.2s, v0\.2s, v1\.2s
**	ret
*/
float32x2_t
full_d (float32x2_t a, float32x2_t b)
{
  return vpadd_f32 (a, b);
}

/*
** full_q64:
**	faddp	v0\.2d, v0\.2d, v1\.2d
**	ret
*/
float64x2_t
full_q64 (float64x2_t a, float64x2_t b)
{
  return vpaddq_f64 (a, b);
}
