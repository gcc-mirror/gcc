/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** f1:
**	...
**	cntb	(x[0-9]+)
**	...
**	add	x[0-9]+, \1, #?16
**	...
**	csel	[^\n]+
**	ret
*/
uint64_t
f1 (int x)
{
  uint64_t y = x ? svcnth () : svcnth () + 8;
  y >>= 3;
  y <<= 4;
  return y;
}

/*
** f2:
**	...
**	(?:and|[al]sr)	[^\n]+
**	...
**	ret
*/
uint64_t
f2 (int x)
{
  uint64_t y = x ? svcnth () : svcnth () + 8;
  y >>= 4;
  y <<= 5;
  return y;
}

/*
** f3:
**	...
**	cntw	(x[0-9]+)
**	...
**	add	x[0-9]+, \1, #?16
**	...
**	csel	[^\n]+
**	ret
*/
uint64_t
f3 (int x)
{
  uint64_t y = x ? svcntd () : svcntd () + 8;
  y >>= 1;
  y <<= 2;
  return y;
}

/*
** f4:
**	...
**	(?:and|[al]sr)	[^\n]+
**	...
**	ret
*/
uint64_t
f4 (int x)
{
  uint64_t y = x ? svcntd () : svcntd () + 8;
  y >>= 2;
  y <<= 3;
  return y;
}
