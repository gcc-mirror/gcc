/* { dg-do compile } */
/* { dg-additional-options "--save-temps -O1" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdint.h>

#pragma GCC target "+cssc"

/*
** ctzw:
**      ctz	w0, w0
**      ret
*/

int32_t
ctzw (int32_t a)
{
  return __builtin_ctz (a);
}

/*
** ctzx:
**      ctz	x0, x0
**      ret
*/

int64_t
ctzx (int64_t a)
{
  return __builtin_ctzll (a);
}

