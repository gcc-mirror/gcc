/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#pragma GCC target "+nosve"

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
**	movi	v[0-9]+.4s, #?0
**	fneg	v[0-9]+.2d, v[0-9]+.2d
**	orr	v[0-9]+.8b, v[0-9]+.8b, v[0-9]+.8b
**	ret
*/
float64_t f2 (float64_t a)
{
  return -fabs (a);
}

