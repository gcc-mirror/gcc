/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#pragma GCC target "+nosve"

#include <string.h>

/*
** negabs: { xfail *-*-* }
**	movi	v31.4s, #?0
**	fneg	v[0-9]+.2d, v[0-9]+.2d
**	orr	v[0-9]+.8b, v[0-9]+.8b, v[0-9]+.8b
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
** negabsf: { xfail *-*-* }
**	movi	v[0-9]+.2s, 0x80, lsl 24
**	orr	v[0-9]+.8b, v[0-9]+.8b, v[0-9]+.8b
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

