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
**	vrshr.u8	q[0-9]+, q[0-9]+, #[0-9]+(?:	@.*|)
**	...
*/
uint8x16_t
foo (uint8x16_t a)
{
  return vrshrq_n_u8 (a, 1);
}


/*
**foo1:
**	...
**	vrshr.u8	q[0-9]+, q[0-9]+, #[0-9]+(?:	@.*|)
**	...
*/
uint8x16_t
foo1 (uint8x16_t a)
{
  return vrshrq (a, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
