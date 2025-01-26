/* { dg-do compile } */
/* { dg-additional-options "-O3 -march=armv9-a+fp8dot4 -moverride=tune=none" } */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {\.L[0-9]+} } } */

#include "arm_neon.h"

/*
** f1:
**	mrs	(x[0-9]+), fpmr
**	cmp	\1, x0
**	beq	([^\n]+)
**	msr	fpmr, x0
** ?\2:
**	fdot	v0.2s, v1.8b, v2.8b
**	ret
*/
float32x2_t
f1 (float32x2_t a, mfloat8x8_t b, mfloat8x8_t c, fpm_t d)
{
  return vdot_f32_mf8_fpm (a, b, c, d);
}

/*
** f2:
**	mrs	(x[0-9]+), fpmr
**	cbz	\1, ([^\n]+)
**	msr	fpmr, xzr
** ?\2:
**	fdot	v0.2s, v1.8b, v2.8b
**	ret
*/
float32x2_t
f2 (float32x2_t a, mfloat8x8_t b, mfloat8x8_t c)
{
  return vdot_f32_mf8_fpm (a, b, c, 0);
}
