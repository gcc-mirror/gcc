/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_neon.h>
#include <math.h>

/*
** f1:
**	orr	z0.s, z0.s, #-2147483648
**	ret
*/
float32_t f1 (float32_t a)
{
  return -fabsf (a);
}

/*
** f2:
**	orr	z0.d, z0.d, #-9223372036854775808
**	ret
*/
float64_t f2 (float64_t a)
{
  return -fabs (a);
}
