/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_neon.h>
#include <math.h>

/*
** f1:
**	...
**	orr	z[0-9]+.s, p[0-9]+/m, z[0-9]+.s, z[0-9]+.s
**	...
*/
void f1 (float32_t *a, int n)
{
  for (int i = 0; i < (n & -8); i++)
   if (a[i] > n)
     a[i] = -fabsf (a[i]);
   else
     a[i] = n;
}

/*
** f2:
**	...
**	orr	z[0-9]+.d, p[0-9]+/m, z[0-9]+.d, z[0-9]+.d
**	...
*/
void f2 (float64_t *a, int n)
{
  for (int i = 0; i < (n & -8); i++)
   if (a[i] > n)
     a[i] = -fabs (a[i]);
   else
     a[i] = n;
}
