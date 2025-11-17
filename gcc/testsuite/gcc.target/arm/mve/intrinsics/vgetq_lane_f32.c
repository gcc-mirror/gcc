/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

float32_t
foo (float32x4_t a)
{
  return vgetq_lane_f32 (a, 1);
}
/*
** foo:
** (
	-mfloat-abi=softfp
**	vmov\td[0-9]+, r0, r1  @ v4sf
**	vmov\td[0-9]+, r2, r3
**	vmov\.32\tr0, q[0-7]\[1\]
** |
	-mfloat-abi=hard
**	vmov\.32\t(r[0-9]+), q0\[1\]
**	vmov\ts0, \1
** )
**	bx\tlr
*/

float32_t
foo1 (float32x4_t a)
{
  return vgetq_lane (a, 1);
}
/*
** foo1:
** (
	-mfloat-abi=softfp
**	vmov\td[0-9]+, r0, r1  @ v4sf
**	vmov\td[0-9]+, r2, r3
**	vmov\.32\tr0, q[0-7]\[1\]
** |
	-mfloat-abi=hard
**	vmov\.32\t(r[0-9]+), q0\[1\]
**	vmov\ts0, \1
** )
**	bx\tlr
*/

#ifdef __cplusplus
}
#endif
