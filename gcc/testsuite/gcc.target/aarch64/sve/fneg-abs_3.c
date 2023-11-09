/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_neon.h>
#include <math.h>

/*
** f1:
**	...
**	ld1w	z[0-9]+.s, p[0-9]+/z, \[x0, x2, lsl 2\]
**	orr	z[0-9]+.s, z[0-9]+.s, #0x80000000
**	st1w	z[0-9]+.s, p[0-9]+, \[x0, x2, lsl 2\]
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
**	ld1d	z[0-9]+.d, p[0-9]+/z, \[x0, x2, lsl 3\]
**	orr	z[0-9]+.d, z[0-9]+.d, #0x8000000000000000
**	st1d	z[0-9]+.d, p[0-9]+, \[x0, x2, lsl 3\]
**	...
*/
void f2 (float64_t *a, int n)
{
  for (int i = 0; i < (n & -8); i++)
   a[i] = -fabs (a[i]);
}
