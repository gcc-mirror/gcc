/* { dg-do compile } */
/* { dg-additional-options "--save-temps -O1" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdint.h>

#pragma GCC target "+cssc"

#define MIN(X, Y) ((X) > (Y) ? (Y) : (X))
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

/*
** uminw:
**      umin	w0, w[01], w[01]
**      ret
*/

uint32_t
uminw (uint32_t a, uint32_t b)
{
  return MIN (a, b);
}

/*
** uminx:
**      umin	x0, x[01], x[01]
**      ret
*/

uint64_t
uminx (uint64_t a, uint64_t b)
{
  return MIN (a, b);
}

/*
** sminw:
**      smin	w0, w[01], w[01]
**      ret
*/

int32_t
sminw (int32_t a, int32_t b)
{
  return MIN (a, b);
}

/*
** sminx:
**      smin	x0, x[01], x[01]
**      ret
*/

int64_t
sminx (int64_t a, int64_t b)
{
  return MIN (a, b);
}

/*
** umaxw:
**      umax	w0, w[01], w[01]
**      ret
*/

uint32_t
umaxw (uint32_t a, uint32_t b)
{
  return MAX (a, b);
}

/*
** umaxx:
**      umax	x0, x[01], x[01]
**      ret
*/

uint64_t
umaxx (uint64_t a, uint64_t b)
{
  return MAX (a, b);
}

/*
** smaxw:
**      smax	w0, w[01], w[01]
**      ret
*/

int32_t
smaxw (int32_t a, int32_t b)
{
  return MAX (a, b);
}

/*
** smaxx:
**      smax	x0, x[01], x[01]
**      ret
*/

int64_t
smaxx (int64_t a, int64_t b)
{
  return MAX (a, b);
}

