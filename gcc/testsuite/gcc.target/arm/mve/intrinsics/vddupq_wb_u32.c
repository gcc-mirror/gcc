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
**	vddup.u32	q[0-9]+, (?:ip|fp|r[0-9]+), #[0-9]+(?:	@.*|)
**	...
*/
uint32x4_t
foo (uint32_t *a)
{
  return vddupq_wb_u32 (a, 1);
}


/*
**foo1:
**	...
**	vddup.u32	q[0-9]+, (?:ip|fp|r[0-9]+), #[0-9]+(?:	@.*|)
**	...
*/
uint32x4_t
foo1 (uint32_t *a)
{
  return vddupq_u32 (a, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
