/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "arm_mve.h"

uint16_t
foo (uint16x8_t a)
{
  return vgetq_lane_u16 (a, 1);
}
/*
** foo:
** (
	-mfloat-abi=softfp
**	vmov\td[0-9]+, r0, r1  @ v8hi
**	vmov\td[0-9]+, r2, r3
**	vmov\.u16\tr0, q[0-7]\[1\]
** |
	-mfloat-abi=hard
**	vmov\.u16\tr0, q0\[1\]
** )
**	bx\tlr
*/

uint16_t
foo1 (uint16x8_t a)
{
  return vgetq_lane (a, 1);
}
/*
** foo1:
** (
	-mfloat-abi=softfp
**	vmov\td[0-9]+, r0, r1  @ v8hi
**	vmov\td[0-9]+, r2, r3
**	vmov\.u16\tr0, q[0-7]\[1\]
** |
	-mfloat-abi=hard
**	vmov\.u16\tr0, q0\[1\]
** )
**	bx\tlr
*/
