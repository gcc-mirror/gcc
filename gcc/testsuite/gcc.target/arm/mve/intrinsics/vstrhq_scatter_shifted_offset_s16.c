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
**	vstrh.16	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+, uxtw #1\](?:	@.*|)
**	...
*/
void
foo (int16_t *base, uint16x8_t offset, int16x8_t value)
{
  vstrhq_scatter_shifted_offset_s16 (base, offset, value);
}


/*
**foo1:
**	...
**	vstrh.16	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+, uxtw #1\](?:	@.*|)
**	...
*/
void
foo1 (int16_t *base, uint16x8_t offset, int16x8_t value)
{
  vstrhq_scatter_shifted_offset (base, offset, value);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
