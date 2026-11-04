/* { dg-do compile } */
/* { dg-require-effective-target stdint_types_mbig_endian } */
/* { dg-require-effective-target arm_arch_v7ve_neon_ok } */
/* { dg-options "-O2 -mbig-endian" } */
/* { dg-add-options arm_arch_v7ve_neon } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>
#include <stdint.h>

int16x4_t be_lane (int16x4_t v)
{
  return vdup_lane_s16 (v, 3);
}

/*
** be_lane:
**	lsrs?	(r[0-9]*), r0, #16
**	vdup.16	d16, \1
**	vmov	r1, r0, d16  @ v4hi
**	bx	lr
*/
