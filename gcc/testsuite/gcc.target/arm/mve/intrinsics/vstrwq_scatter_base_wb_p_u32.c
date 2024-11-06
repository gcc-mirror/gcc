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
**	vstrwt.u32	q[0-9]+, \[q[0-9]+, #[0-9]+\]!(?:	@.*|)
**	...
*/
void
foo (uint32x4_t *addr, uint32x4_t value, mve_pred16_t p)
{
  vstrwq_scatter_base_wb_p_u32 (addr, 0, value, p);
}


/*
**foo1:
**	...
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vstrwt.u32	q[0-9]+, \[q[0-9]+, #[0-9]+\]!(?:	@.*|)
**	...
*/
void
foo1 (uint32x4_t *addr, uint32x4_t value, mve_pred16_t p)
{
  vstrwq_scatter_base_wb_p (addr, 0, value, p);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
