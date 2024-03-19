/* { dg-do assemble } */
/* { dg-options "-O --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdint.h>

/*
** fpr_to_fpr:
**	fmov	s0, s1
**	ret
*/
void
fpr_to_fpr (void) [[arm::streaming_compatible]]
{
  register uint8_t q0 asm ("q0");
  register uint8_t q1 asm ("q1");
  asm volatile ("" : "=w" (q1));
  q0 = q1;
  asm volatile ("" :: "w" (q0));
}

/*
** gpr_to_fpr:
**	fmov	s0, w0
**	ret
*/
void
gpr_to_fpr (uint8_t w0) [[arm::streaming_compatible]]
{
  register uint8_t q0 asm ("q0");
  q0 = w0;
  asm volatile ("" :: "w" (q0));
}

/*
** zero_to_fpr:
**	fmov	s0, wzr
**	ret
*/
void
zero_to_fpr () [[arm::streaming_compatible]]
{
  register uint8_t q0 asm ("q0");
  q0 = 0;
  asm volatile ("" :: "w" (q0));
}

/*
** fpr_to_gpr:
**	umov	w0, v0.b\[0\]
**	ret
*/
uint8_t
fpr_to_gpr () [[arm::streaming_compatible]]
{
  register uint8_t q0 asm ("q0");
  asm volatile ("" : "=w" (q0));
  return q0;
}
