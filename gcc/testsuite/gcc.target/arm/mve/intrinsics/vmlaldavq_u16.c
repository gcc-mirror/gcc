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
**	vmlaldav.u16	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
uint64_t
foo (uint16x8_t m1, uint16x8_t m2)
{
  return vmlaldavq_u16 (m1, m2);
}


/*
**foo1:
**	...
**	vmlaldav.u16	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
uint64_t
foo1 (uint16x8_t m1, uint16x8_t m2)
{
  return vmlaldavq (m1, m2);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
