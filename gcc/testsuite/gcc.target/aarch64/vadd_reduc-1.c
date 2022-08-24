/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

typedef int v4si __attribute__ ((vector_size (16)));

/*
**bar:
**	...
**	addv	s0, v0.4s
**	fmov	w0, s0
**	lsr	w1, w0, 16
**	add	w0, w1, w0, uxth
**	ret
*/
int bar (v4si x)
{
  unsigned int sum = vaddvq_s32 (x);
  return (((uint16_t)(sum & 0xffff)) + ((uint32_t)sum >> 16));
}
