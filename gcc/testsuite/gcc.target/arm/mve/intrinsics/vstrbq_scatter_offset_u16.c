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
**	vstrb.16	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+\](?:	@.*|)
**	...
*/
void
foo (uint8_t *base, uint16x8_t offset, uint16x8_t value)
{
  vstrbq_scatter_offset_u16 (base, offset, value);
}


/*
**foo1:
**	...
**	vstrb.16	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+\](?:	@.*|)
**	...
*/
void
foo1 (uint8_t *base, uint16x8_t offset, uint16x8_t value)
{
  vstrbq_scatter_offset (base, offset, value);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
