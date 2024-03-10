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
**	vmla.u32	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint32x4_t
foo (uint32x4_t add, uint32x4_t m1, uint32_t m2)
{
  return vmlaq_n_u32 (add, m1, m2);
}


/*
**foo1:
**	...
**	vmla.u32	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint32x4_t
foo1 (uint32x4_t add, uint32x4_t m1, uint32_t m2)
{
  return vmlaq (add, m1, m2);
}

/*
**foo2:
**	...
**	vmla.u32	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint32x4_t
foo2 (uint32x4_t add, uint32x4_t m1)
{
  return vmlaq (add, m1, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
