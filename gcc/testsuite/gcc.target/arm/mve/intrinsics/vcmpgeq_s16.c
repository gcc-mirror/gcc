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
**	vcmp.s16	ge, q[0-9]+, q[0-9]+(?:	@.*|)
**	...
**	vmrs	(?:ip|fp|r[0-9]+), p0(?:	@.*|)
**	...
*/
mve_pred16_t
foo (int16x8_t a, int16x8_t b)
{
  return vcmpgeq_s16 (a, b);
}


/*
**foo1:
**	...
**	vcmp.s16	ge, q[0-9]+, q[0-9]+(?:	@.*|)
**	...
**	vmrs	(?:ip|fp|r[0-9]+), p0(?:	@.*|)
**	...
*/
mve_pred16_t
foo1 (int16x8_t a, int16x8_t b)
{
  return vcmpgeq (a, b);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */