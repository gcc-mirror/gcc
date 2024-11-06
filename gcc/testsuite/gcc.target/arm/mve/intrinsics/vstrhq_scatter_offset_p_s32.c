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
**	vstrht.32	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+\](?:	@.*|)
**	...
*/
void
foo (int16_t *base, uint32x4_t offset, int32x4_t value, mve_pred16_t p)
{
  vstrhq_scatter_offset_p_s32 (base, offset, value, p);
}


/*
**foo1:
**	...
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vstrht.32	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+\](?:	@.*|)
**	...
*/
void
foo1 (int16_t *base, uint32x4_t offset, int32x4_t value, mve_pred16_t p)
{
  vstrhq_scatter_offset_p (base, offset, value, p);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
