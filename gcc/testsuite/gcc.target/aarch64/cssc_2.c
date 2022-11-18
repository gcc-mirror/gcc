/* { dg-do compile } */
/* { dg-additional-options "--save-temps -O1" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdint.h>

#pragma GCC target "+cssc"

/*
** cntw:
**      cnt	w0, w0
**      ret
*/

int32_t
cntw (int32_t a)
{
  return __builtin_popcount (a);
}

/*
** cntx:
**      cnt	x0, x0
**      ret
*/

int64_t
cntx (int64_t a)
{
  return __builtin_popcountll (a);
}

