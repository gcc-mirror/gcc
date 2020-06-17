/* { dg-do compile } */
/* { dg-options "-O -msve-vector-bits=128 -g" } */
/* { dg-final { check-function-bodies "**" "" "" { target { aarch64_little_endian && lp64 } } } } */

/*
** callee_pred:
**	ldr	p0, \[x0\]
**	ret
*/
__SVBool_t __attribute__((noipa))
callee_pred (__SVBool_t *ptr)
{
  return *ptr;
}

#include <arm_sve.h>

/*
** caller_pred:
**	...
**	bl	callee_pred
**	cntp	x0, p0, p0.b
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
uint64_t __attribute__((noipa))
caller_pred (__SVBool_t *ptr1)
{
  __SVBool_t p = callee_pred (ptr1);
  return svcntp_b8 (p, p);
}
