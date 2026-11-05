/* { dg-do compile } */
/* { dg-require-effective-target stdint_types_mbig_endian } */
/* { dg-require-effective-target arm_arch_v7a_neon_hard_ok } */
/* { dg-options "-O3 -mbig-endian" } */
/* { dg-add-options arm_arch_v7a_neon_hard } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>
#include <stdint.h>

int16x8_t be_dupq_lane (int16x8_t v)
{
  return (int16x8_t) __builtin_shuffle (v, v,
					(uint16x8_t) { 0, 0, 0, 0,
						       0, 0, 0, 0 });
}

/*
** be_dupq_lane:
**	vdup.16	q0, d0\[3\]
**	bx	lr
*/
