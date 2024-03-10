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
**	vbict.i32	q[0-9]+, #[0-9]+(?:	@.*|)
**	...
*/
uint32x4_t
foo (uint32x4_t a, mve_pred16_t p)
{
  return vbicq_m_n_u32 (a, 1, p);
}


/*
**foo1:
**	...
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vbict.i32	q[0-9]+, #[0-9]+(?:	@.*|)
**	...
*/
uint32x4_t
foo1 (uint32x4_t a, mve_pred16_t p)
{
  return vbicq_m_n (a, 1, p);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
