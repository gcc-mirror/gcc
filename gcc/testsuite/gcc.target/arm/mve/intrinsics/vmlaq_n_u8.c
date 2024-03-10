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
**	vmla.u8	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint8x16_t
foo (uint8x16_t add, uint8x16_t m1, uint8_t m2)
{
  return vmlaq_n_u8 (add, m1, m2);
}


/*
**foo1:
**	...
**	vmla.u8	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint8x16_t
foo1 (uint8x16_t add, uint8x16_t m1, uint8_t m2)
{
  return vmlaq (add, m1, m2);
}

/*
**foo2:
**	...
**	vmla.u8	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint8x16_t
foo2 (uint8x16_t add, uint8x16_t m1)
{
  return vmlaq (add, m1, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
