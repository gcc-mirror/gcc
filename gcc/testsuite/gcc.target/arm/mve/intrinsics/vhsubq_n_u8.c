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
**	vhsub.u8	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint8x16_t
foo (uint8x16_t a, uint8_t b)
{
  return vhsubq_n_u8 (a, b);
}


/*
**foo1:
**	...
**	vhsub.u8	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint8x16_t
foo1 (uint8x16_t a, uint8_t b)
{
  return vhsubq (a, b);
}

/*
**foo2:
**	...
**	vhsub.u8	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint8x16_t
foo2 (uint8x16_t a)
{
  return vhsubq (a, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */