/* { dg-do compile } */
/* { dg-options "-O -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** callee_pred:
**	ldr	p0, \[x0\]
**	ret
*/
svbool_t __attribute__((noipa))
callee_pred (svbool_t *ptr)
{
  return *ptr;
}

/*
** caller_pred:
**	...
**	bl	callee_pred
**	cntp	x0, p0, p0.b
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
uint64_t __attribute__((noipa))
caller_pred (svbool_t *ptr1)
{
  svbool_t p;
  p = callee_pred (ptr1);
  return svcntp_b8 (p, p);
}
