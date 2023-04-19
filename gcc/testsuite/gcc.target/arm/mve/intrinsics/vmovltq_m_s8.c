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
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vmovltt.s8	q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
int16x8_t
foo (int16x8_t inactive, int8x16_t a, mve_pred16_t p)
{
  return vmovltq_m_s8 (inactive, a, p);
}


/*
**foo1:
**	...
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vmovltt.s8	q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
int16x8_t
foo1 (int16x8_t inactive, int8x16_t a, mve_pred16_t p)
{
  return vmovltq_m (inactive, a, p);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
