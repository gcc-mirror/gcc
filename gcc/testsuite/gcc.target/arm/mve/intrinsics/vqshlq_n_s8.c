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
**	vqshl.s8	q[0-9]+, q[0-9]+, #[0-9]+(?:	@.*|)
**	...
*/
int8x16_t
foo (int8x16_t a)
{
  return vqshlq_n_s8 (a, 1);
}


/*
**foo1:
**	...
**	vqshl.s8	q[0-9]+, q[0-9]+, #[0-9]+(?:	@.*|)
**	...
*/
int8x16_t
foo1 (int8x16_t a)
{
  return vqshlq_n (a, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
