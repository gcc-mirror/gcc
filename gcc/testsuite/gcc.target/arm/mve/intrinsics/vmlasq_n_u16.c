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
**	vmlas.u16	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint16x8_t
foo (uint16x8_t m1, uint16x8_t m2, uint16_t add)
{
  return vmlasq_n_u16 (m1, m2, add);
}


/*
**foo1:
**	...
**	vmlas.u16	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint16x8_t
foo1 (uint16x8_t m1, uint16x8_t m2, uint16_t add)
{
  return vmlasq (m1, m2, add);
}

/*
**foo2:
**	...
**	vmlas.u16	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint16x8_t
foo2 (uint16x8_t m1, uint16x8_t m2)
{
  return vmlasq (m1, m2, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */