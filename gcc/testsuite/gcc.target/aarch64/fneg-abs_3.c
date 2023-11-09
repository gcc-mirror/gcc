/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#pragma GCC target "+nosve"

#include <arm_neon.h>
#include <math.h>

/*
** f1:
**	...
**	ldr	q[0-9]+, \[x0\]
**	orr	v[0-9]+.4s, #128, lsl #24
**	str	q[0-9]+, \[x0\], 16
**	...
*/
void f1 (float32_t *a, int n)
{
  for (int i = 0; i < (n & -8); i++)
   a[i] = -fabsf (a[i]);
}

/*
** f2:
**	...
**	ldr	q[0-9]+, \[x0\]
**	orr	v[0-9]+.16b, v[0-9]+.16b, v[0-9]+.16b
**	str	q[0-9]+, \[x0\], 16
**	...
*/
void f2 (float64_t *a, int n)
{
  for (int i = 0; i < (n & -8); i++)
   a[i] = -fabs (a[i]);
}
