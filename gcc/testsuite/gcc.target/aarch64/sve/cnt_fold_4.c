/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** f1:
**	cnth	x0
**	ret
*/
uint64_t
f1 ()
{
  uint64_t x = svcntw ();
  x >>= 2;
  return x << 3;
}

/*
** f2:
**	[^\n]+
**	[^\n]+
**	...
**	ret
*/
uint64_t
f2 ()
{
  uint64_t x = svcntd ();
  x >>= 2;
  return x << 3;
}

/*
** f3:
**	cntb	x0, all, mul #4
**	ret
*/
uint64_t
f3 ()
{
  uint64_t x = svcntd ();
  x >>= 1;
  return x << 6;
}

/*
** f4:
**	[^\n]+
**	[^\n]+
**	...
**	ret
*/
uint64_t
f4 ()
{
  uint64_t x = svcntd ();
  x >>= 2;
  return x << 2;
}
