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
**	vldrw.u32	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+, uxtw #2\](?:	@.*|)
**	...
*/
uint32x4_t
foo (uint32_t const *base, uint32x4_t offset)
{
  return vldrwq_gather_shifted_offset_u32 (base, offset);
}


/*
**foo1:
**	...
**	vldrw.u32	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+, uxtw #2\](?:	@.*|)
**	...
*/
uint32x4_t
foo1 (uint32_t const *base, uint32x4_t offset)
{
  return vldrwq_gather_shifted_offset (base, offset);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
