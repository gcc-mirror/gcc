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
**	vshllb.s16	q[0-9]+, q[0-9]+, #[0-9]+(?:	@.*|)
**	...
*/
int32x4_t
foo (int16x8_t a)
{
  return vshllbq_n_s16 (a, 1);
}


/*
**foo1:
**	...
**	vshllb.s16	q[0-9]+, q[0-9]+, #[0-9]+(?:	@.*|)
**	...
*/
int32x4_t
foo1 (int16x8_t a)
{
  return vshllbq (a, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
