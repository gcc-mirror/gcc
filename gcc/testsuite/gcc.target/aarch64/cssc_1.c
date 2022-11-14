/* { dg-do compile } */
/* { dg-additional-options "--save-temps -O1" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdint.h>

#pragma GCC target "+cssc"

/*
** absw:
**      abs	w0, w0
**      ret
*/

int32_t
absw (int32_t a)
{
  return __builtin_abs (a);
}

/*
** absx:
**      abs	x0, x0
**      ret
*/

int64_t
absx (int64_t a)
{
  return __builtin_labs (a);
}

