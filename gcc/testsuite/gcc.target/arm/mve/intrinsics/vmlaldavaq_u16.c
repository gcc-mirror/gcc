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
**	vmlaldava.u16	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
uint64_t
foo (uint64_t add, uint16x8_t m1, uint16x8_t m2)
{
  return vmlaldavaq_u16 (add, m1, m2);
}


/*
**foo1:
**	...
**	vmlaldava.u16	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
uint64_t
foo1 (uint64_t add, uint16x8_t m1, uint16x8_t m2)
{
  return vmlaldavaq (add, m1, m2);
}

/*
**foo2:
**	...
**	vmlaldava.u16	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
uint64_t
foo2 (uint16x8_t m1, uint16x8_t m2)
{
  return vmlaldavaq (1, m1, m2);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
