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
**	vidupt.u16	q[0-9]+, (?:ip|fp|r[0-9]+), #[0-9]+(?:	@.*|)
**	...
*/
uint16x8_t
foo (uint16x8_t inactive, uint32_t *a, mve_pred16_t p)
{
  return vidupq_m_wb_u16 (inactive, a, 1, p);
}


/*
**foo1:
**	...
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vidupt.u16	q[0-9]+, (?:ip|fp|r[0-9]+), #[0-9]+(?:	@.*|)
**	...
*/
uint16x8_t
foo1 (uint16x8_t inactive, uint32_t *a, mve_pred16_t p)
{
  return vidupq_m (inactive, a, 1, p);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
