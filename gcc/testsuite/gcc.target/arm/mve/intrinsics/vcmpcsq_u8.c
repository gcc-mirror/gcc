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
**	vcmp.u8	cs, q[0-9]+, q[0-9]+(?:	@.*|)
**	...
**	vmrs	(?:ip|fp|r[0-9]+), p0(?:	@.*|)
**	...
*/
mve_pred16_t
foo (uint8x16_t a, uint8x16_t b)
{
  return vcmpcsq_u8 (a, b);
}


/*
**foo1:
**	...
**	vcmp.u8	cs, q[0-9]+, q[0-9]+(?:	@.*|)
**	...
**	vmrs	(?:ip|fp|r[0-9]+), p0(?:	@.*|)
**	...
*/
mve_pred16_t
foo1 (uint8x16_t a, uint8x16_t b)
{
  return vcmpcsq (a, b);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */