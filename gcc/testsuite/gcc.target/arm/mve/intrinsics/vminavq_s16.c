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
**	vminav.s16	(?:ip|fp|r[0-9]+), q[0-9]+(?:	@.*|)
**	...
*/
uint16_t
foo (uint16_t a, int16x8_t b)
{
  return vminavq_s16 (a, b);
}


/*
**foo1:
**	...
**	vminav.s16	(?:ip|fp|r[0-9]+), q[0-9]+(?:	@.*|)
**	...
*/
uint16_t
foo1 (uint16_t a, int16x8_t b)
{
  return vminavq (a, b);
}

/*
**foo2:
**	...
**	vminav.s16	(?:ip|fp|r[0-9]+), q[0-9]+(?:	@.*|)
**	...
*/
uint16_t
foo2 (int16x8_t b)
{
  return vminavq (1, b);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */