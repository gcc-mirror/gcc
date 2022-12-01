/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
**foo:
**	...
**	vqdmulh.s32	q[0-9]+, q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
int32x4_t
foo (int32x4_t a, int32x4_t b)
{
  return vqdmulhq_s32 (a, b);
}


/*
**foo1:
**	...
**	vqdmulh.s32	q[0-9]+, q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
int32x4_t
foo1 (int32x4_t a, int32x4_t b)
{
  return vqdmulhq (a, b);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */