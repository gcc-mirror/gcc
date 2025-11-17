/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

uint64_t
foo (uint64x2_t a)
{
  return vgetq_lane_u64 (a, 0);
}
/*
** foo:
** (
	-mfloat-abi=softfp
	no operation needed
** |
	-mfloat-abi=hard
**	vmov\tr0, r1, d0\t@ int
** )
**	bx\tlr
*/

uint64_t
foo1 (uint64x2_t a)
{
  return vgetq_lane (a, 0);
}
/*
** foo1:
** (
	-mfloat-abi=softfp
	no operation needed
** |
	-mfloat-abi=hard
**	vmov\tr0, r1, d0\t@ int
** )
**	bx\tlr
*/

#ifdef __cplusplus
}
#endif
