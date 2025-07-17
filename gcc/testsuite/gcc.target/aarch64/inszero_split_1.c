/* { dg-do compile } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Avoid INS from WZR register when optimizing for speed.  */

#include <arm_neon.h>

/*
** foo:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	ins	v0.h\[2\], v(\1).h\[0\]
**	ret
*/
uint16x8_t foo(uint16x8_t a) {
  a[2] = 0;
  return a;
}
