/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <string.h>

/*
** negabs:
**	orr	z0.d, z0.d, #?-9223372036854775808
**	ret
*/
double negabs (double x)
{
   unsigned long long y;
   memcpy (&y, &x, sizeof(double));
   y = y | (1UL << 63);
   memcpy (&x, &y, sizeof(double));
   return x;
}

/*
** negabsf:
**	orr	v0.4s, #?128, lsl #?24
**	ret
*/
float negabsf (float x)
{
   unsigned int y;
   memcpy (&y, &x, sizeof(float));
   y = y | (1U << 31);
   memcpy (&x, &y, sizeof(float));
   return x;
}

