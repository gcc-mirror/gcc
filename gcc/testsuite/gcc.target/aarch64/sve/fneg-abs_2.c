/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_neon.h>
#include <math.h>

/*
** f1:
**	movi	v[0-9]+.2s, 0x80, lsl 24
**	orr	v[0-9]+.8b, v[0-9]+.8b, v[0-9]+.8b
**	ret
*/
float32_t f1 (float32_t a)
{
  return -fabsf (a);
}

/*
** f2:
**	mov	x0, -9223372036854775808
**	fmov	d[0-9]+, x0
**	orr	v[0-9]+.8b, v[0-9]+.8b, v[0-9]+.8b
**	ret
*/
float64_t f2 (float64_t a)
{
  return -fabs (a);
}
