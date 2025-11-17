/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

int32_t
foo (int32x4_t a)
{
  return vgetq_lane_s32 (a, 1);
}
/*
** foo:
** (
	-mfloat-abi=softfp
**	vmov\td[0-9]+, r0, r1  @ v4si
**	vmov\td[0-9]+, r2, r3
**	vmov\.32\tr0, q[0-7]\[1\]
** |
	-mfloat-abi=hard
**	vmov\.32\tr0, q0\[1\]
** )
**	bx\tlr
*/

int32_t
foo1 (int32x4_t a)
{
  return vgetq_lane (a, 1);
}
/*
** foo1:
** (
	-mfloat-abi=softfp
**	vmov\td[0-9]+, r0, r1  @ v4si
**	vmov\td[0-9]+, r2, r3
**	vmov\.32\tr0, q[0-7]\[1\]
** |
	-mfloat-abi=hard
**	vmov\.32\tr0, q0\[1\]
** )
**	bx\tlr
*/

#ifdef __cplusplus
}
#endif
