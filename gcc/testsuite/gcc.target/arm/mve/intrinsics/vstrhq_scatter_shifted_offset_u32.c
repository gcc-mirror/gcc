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
**	vstrh.32	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+, uxtw #1\](?:	@.*|)
**	...
*/
void
foo (uint16_t *base, uint32x4_t offset, uint32x4_t value)
{
  return vstrhq_scatter_shifted_offset_u32 (base, offset, value);
}


/*
**foo1:
**	...
**	vstrh.32	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+, uxtw #1\](?:	@.*|)
**	...
*/
void
foo1 (uint16_t *base, uint32x4_t offset, uint32x4_t value)
{
  return vstrhq_scatter_shifted_offset (base, offset, value);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
