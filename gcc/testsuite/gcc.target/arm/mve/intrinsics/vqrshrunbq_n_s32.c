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
**	vqrshrunb.s32	q[0-9]+, q[0-9]+, #[0-9]+(?:	@.*|)
**	...
*/
uint16x8_t
foo (uint16x8_t a, int32x4_t b)
{
  return vqrshrunbq_n_s32 (a, b, 1);
}


/*
**foo1:
**	...
**	vqrshrunb.s32	q[0-9]+, q[0-9]+, #[0-9]+(?:	@.*|)
**	...
*/
uint16x8_t
foo1 (uint16x8_t a, int32x4_t b)
{
  return vqrshrunbq (a, b, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
