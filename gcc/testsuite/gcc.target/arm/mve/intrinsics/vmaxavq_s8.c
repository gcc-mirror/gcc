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
**	vmaxav.s8	(?:ip|fp|r[0-9]+), q[0-9]+(?:	@.*|)
**	...
*/
uint8_t
foo (uint8_t a, int8x16_t b)
{
  return vmaxavq_s8 (a, b);
}


/*
**foo1:
**	...
**	vmaxav.s8	(?:ip|fp|r[0-9]+), q[0-9]+(?:	@.*|)
**	...
*/
uint8_t
foo1 (uint8_t a, int8x16_t b)
{
  return vmaxavq (a, b);
}

/*
**foo2:
**	...
**	vmaxav.s8	(?:ip|fp|r[0-9]+), q[0-9]+(?:	@.*|)
**	...
*/
uint8_t
foo2 (int8x16_t b)
{
  return vmaxavq (1, b);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */