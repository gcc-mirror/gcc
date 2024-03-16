/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_neon.h>

/*
** t1:
**	orr	z[0-9]+.s, z[0-9]+.s, #-2147483648
**	ret
*/
float32x2_t t1 (float32x2_t a)
{
  return vneg_f32 (vabs_f32 (a));
}

/*
** t2:
**	orr	z[0-9]+.s, z[0-9]+.s, #-2147483648
**	ret
*/
float32x4_t t2 (float32x4_t a)
{
  return vnegq_f32 (vabsq_f32 (a));
}

/*
** t3:
**	orr	z[0-9]+.d, z[0-9]+.d, #-9223372036854775808
**	ret
*/
float64x2_t t3 (float64x2_t a)
{
  return vnegq_f64 (vabsq_f64 (a));
}
