/* { dg-do assemble } */
/* { dg-options "-O --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target "+nothing+nosimd+fp"

#include <stdint.h>

/*
** fpr_to_fpr:
**	fmov	d0, d1
**	ret
*/
void
fpr_to_fpr (void)
{
  register uint64_t q0 asm ("q0");
  register uint64_t q1 asm ("q1");
  asm volatile ("" : "=w" (q1));
  q0 = q1;
  asm volatile ("" :: "w" (q0));
}

/*
** gpr_to_fpr:
**	fmov	d0, x0
**	ret
*/
void
gpr_to_fpr (uint64_t x0)
{
  register uint64_t q0 asm ("q0");
  q0 = x0;
  asm volatile ("" :: "w" (q0));
}

/*
** zero_to_fpr:
**	fmov	d0, xzr
**	ret
*/
void
zero_to_fpr ()
{
  register uint64_t q0 asm ("q0");
  q0 = 0;
  asm volatile ("" :: "w" (q0));
}

/*
** fpr_to_gpr:
**	fmov	x0, d0
**	ret
*/
uint64_t
fpr_to_gpr ()
{
  register uint64_t q0 asm ("q0");
  asm volatile ("" : "=w" (q0));
  return q0;
}
