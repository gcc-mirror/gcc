/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** f1:
**	...
**	cntd	[^\n]+
**	...
**	mul	[^\n]+
**	ret
*/
uint64_t
f1 (int x)
{
  if (x & 3)
    __builtin_unreachable ();
  x >>= 2;
  return (uint64_t) x * svcnth ();
}

/*
** f2:
**	...
**	asr	[^\n]+
**	...
**	ret
*/
uint64_t
f2 (int x)
{
  if (x & 3)
    __builtin_unreachable ();
  x >>= 2;
  return (uint64_t) x * svcntw ();
}
