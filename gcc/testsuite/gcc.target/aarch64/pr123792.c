/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdint.h>

/*
** f1:
**	mvn	w1, w1
**	lsl	w0, w0, w1
**	ret
*/

int f1 (int x, int n)
{
  return x << (31 - n);
}

/*
** f2:
**	mvn	w1, w1
**	asr	w0, w0, w1
**	ret
*/

int f2 (int x, int n)
{
  return x >> (31 - n);
}

/*
** f3:
**	mvn	w1, w1
**	lsr	x0, x0, x1
**	ret
*/

unsigned long f3 (unsigned long long x, int n)
{
  return x >> (63 - n);
}

/*
** f4:
**	mvn	w1, w1
**	lsl	x0, x0, x1
**	ret
*/

long f4 (long x, int n)
{
  return x << (63 - n);
}
