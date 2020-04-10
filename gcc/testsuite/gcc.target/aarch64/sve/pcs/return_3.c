/* { dg-do compile } */
/* { dg-options "-O -g" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_sve.h>

typedef svbool_t my_pred;

/*
** callee_pred:
**	ldr	p0, \[x0\]
**	ret
*/
my_pred __attribute__((noipa))
callee_pred (my_pred *ptr)
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
caller_pred (my_pred *ptr1)
{
  my_pred p;
  p = callee_pred (ptr1);
  return svcntp_b8 (p, p);
}
