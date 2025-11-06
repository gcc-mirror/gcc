/* { dg-do compile } */
/* { dg-additional-options "--save-temps -O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdint.h>

#pragma GCC target "+nocssc"

/*
** ffsw1:
**	cmp	w1, 0
**	rbit	w0, w1
**	clz	w0, w0
**	csinc	w0, wzr, w0, eq
**	ret
*/

int ffsw1 (int y, uint32_t x)
{
  return __builtin_ffs (x);
}

/*
** ffsx1:
**	cmp	x1, 0
**	rbit	x0, x1
**	clz	x0, x0
**	csinc	x0, xzr, x0, eq
**	ret
*/

int ffsx1 (int y, uint64_t x)
{
  return __builtin_ffsll (x);
}

#pragma GCC target "+cssc"

/*
** ffsw2:
**	cmp	w1, 0
**	ctz	w0, w1
**	csinc	w0, wzr, w0, eq
**	ret
*/

int ffsw2 (int y, uint32_t x)
{
  return __builtin_ffs (x);
}

/*
** ffsx2:
**	cmp	x1, 0
**	ctz	x0, x1
**	csinc	x0, xzr, x0, eq
**	ret
*/

int ffsx2 (int y, uint64_t x)
{
  return __builtin_ffsll (x);
}
