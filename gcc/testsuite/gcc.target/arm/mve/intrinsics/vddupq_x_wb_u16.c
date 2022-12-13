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
**	vddupt.u16	q[0-9]+, (?:ip|fp|r[0-9]+), #[0-9]+(?:	@.*|)
**	...
*/
uint16x8_t
foo (uint32_t *a, mve_pred16_t p)
{
  return vddupq_x_wb_u16 (a, 1, p);
}


/*
**foo1:
**	...
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vddupt.u16	q[0-9]+, (?:ip|fp|r[0-9]+), #[0-9]+(?:	@.*|)
**	...
*/
uint16x8_t
foo1 (uint32_t *a, mve_pred16_t p)
{
  return vddupq_x_u16 (a, 1, p);
}

/*
**foo2:
**	...
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vddupt.u16	q[0-9]+, (?:ip|fp|r[0-9]+), #[0-9]+(?:	@.*|)
**	...
*/
uint16x8_t
foo2 (mve_pred16_t p)
{
  return vddupq_x_u16 (1, 1, p);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */