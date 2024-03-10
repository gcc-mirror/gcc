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
**	vstrd.u64	q[0-9]+, \[q[0-9]+, #[0-9]+\]!(?:	@.*|)
**	...
*/
void
foo (uint64x2_t *addr, uint64x2_t value)
{
  return vstrdq_scatter_base_wb_u64 (addr, 0, value);
}


/*
**foo1:
**	...
**	vstrd.u64	q[0-9]+, \[q[0-9]+, #[0-9]+\]!(?:	@.*|)
**	...
*/
void
foo1 (uint64x2_t *addr, uint64x2_t value)
{
  return vstrdq_scatter_base_wb (addr, 0, value);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
