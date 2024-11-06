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
**	vstrd.64	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+\](?:	@.*|)
**	...
*/
void
foo (uint64_t *base, uint64x2_t offset, uint64x2_t value)
{
  vstrdq_scatter_offset_u64 (base, offset, value);
}


/*
**foo1:
**	...
**	vstrd.64	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+\](?:	@.*|)
**	...
*/
void
foo1 (uint64_t *base, uint64x2_t offset, uint64x2_t value)
{
  vstrdq_scatter_offset (base, offset, value);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
