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
**	vstrw.32	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+, uxtw #2\](?:	@.*|)
**	...
*/
void
foo (int32_t *base, uint32x4_t offset, int32x4_t value)
{
  return vstrwq_scatter_shifted_offset_s32 (base, offset, value);
}


/*
**foo1:
**	...
**	vstrw.32	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+, uxtw #2\](?:	@.*|)
**	...
*/
void
foo1 (int32_t *base, uint32x4_t offset, int32x4_t value)
{
  return vstrwq_scatter_shifted_offset (base, offset, value);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
