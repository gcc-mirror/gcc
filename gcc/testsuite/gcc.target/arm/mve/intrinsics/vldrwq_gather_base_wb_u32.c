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
**	vldrw.u32	q[0-9]+, \[q[0-9]+, #[0-9]+\]!(?:	@.*|)
**	...
*/
uint32x4_t
foo (uint32x4_t *addr)
{
  return vldrwq_gather_base_wb_u32 (addr, 0);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
