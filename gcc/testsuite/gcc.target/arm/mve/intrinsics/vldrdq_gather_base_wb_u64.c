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
**	vldrd.64	q[0-9]+, \[q[0-9]+, #[0-9]+\]!(?:	@.*|)
**	...
*/
uint64x2_t
foo (uint64x2_t *addr)
{
  return vldrdq_gather_base_wb_u64 (addr, 0);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */