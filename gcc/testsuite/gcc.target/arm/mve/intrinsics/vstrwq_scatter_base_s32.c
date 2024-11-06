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
**	vstrw.u32	q[0-9]+, \[q[0-9]+, #[0-9]+\](?:	@.*|)
**	...
*/
void
foo (uint32x4_t addr, int32x4_t value)
{
  vstrwq_scatter_base_s32 (addr, 0, value);
}


/*
**foo1:
**	...
**	vstrw.u32	q[0-9]+, \[q[0-9]+, #[0-9]+\](?:	@.*|)
**	...
*/
void
foo1 (uint32x4_t addr, int32x4_t value)
{
  vstrwq_scatter_base (addr, 0, value);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
