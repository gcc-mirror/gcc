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
**	vminv.u16	(?:ip|fp|r[0-9]+), q[0-9]+(?:	@.*|)
**	...
*/
uint16_t
foo (uint16_t a, uint16x8_t b)
{
  return vminvq_u16 (a, b);
}


/*
**foo1:
**	...
**	vminv.u16	(?:ip|fp|r[0-9]+), q[0-9]+(?:	@.*|)
**	...
*/
uint16_t
foo1 (uint16_t a, uint16x8_t b)
{
  return vminvq (a, b);
}

/*
**foo2:
**	...
**	vminv.u16	(?:ip|fp|r[0-9]+), q[0-9]+(?:	@.*|)
**	...
*/
uint16_t
foo2 (uint16x8_t b)
{
  return vminvq (1, b);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */