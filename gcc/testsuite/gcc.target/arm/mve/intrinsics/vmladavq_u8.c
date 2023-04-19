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
**	vmladav.u8	(?:ip|fp|r[0-9]+), q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
uint32_t
foo (uint8x16_t m1, uint8x16_t m2)
{
  return vmladavq_u8 (m1, m2);
}


/*
**foo1:
**	...
**	vmladav.u8	(?:ip|fp|r[0-9]+), q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
uint32_t
foo1 (uint8x16_t m1, uint8x16_t m2)
{
  return vmladavq (m1, m2);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
