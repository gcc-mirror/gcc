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
**	vstrdt.64	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+, uxtw #3\](?:	@.*|)
**	...
*/
void
foo (uint64_t *base, uint64x2_t offset, uint64x2_t value, mve_pred16_t p)
{
  vstrdq_scatter_shifted_offset_p_u64 (base, offset, value, p);
}


/*
**foo1:
**	...
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vstrdt.64	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+, uxtw #3\](?:	@.*|)
**	...
*/
void
foo1 (uint64_t *base, uint64x2_t offset, uint64x2_t value, mve_pred16_t p)
{
  vstrdq_scatter_shifted_offset_p (base, offset, value, p);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
