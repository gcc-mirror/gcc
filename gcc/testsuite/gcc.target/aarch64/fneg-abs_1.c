/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#pragma GCC target "+nosve"

#include <arm_neon.h>

/*
** t1:
**	orr	v[0-9]+.2s, #128, lsl #24
**	ret
*/
float32x2_t t1 (float32x2_t a)
{
  return vneg_f32 (vabs_f32 (a));
}

/*
** t2:
**	orr	v[0-9]+.4s, #128, lsl #24
**	ret
*/
float32x4_t t2 (float32x4_t a)
{
  return vnegq_f32 (vabsq_f32 (a));
}

/*
** t3:
**	movi	v[0-9]+.4s, 0
**	fneg	v[0-9]+.2d, v[0-9]+.2d
**	orr	v[0-9]+.16b, v[0-9]+.16b, v[0-9]+.16b
**	ret
*/
float64x2_t t3 (float64x2_t a)
{
  return vnegq_f64 (vabsq_f64 (a));
}
