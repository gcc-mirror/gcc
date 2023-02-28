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
**	vldrb.8	q[0-9]+, \[(?:ip|fp|r[0-9]+)\](?:	@.*|)
**	...
*/
int8x16_t
foo (int8_t const *base)
{
  return vld1q_s8 (base);
}


/*
**foo1:
**	...
**	vldrb.8	q[0-9]+, \[(?:ip|fp|r[0-9]+)\](?:	@.*|)
**	...
*/
int8x16_t
foo1 (int8_t const *base)
{
  return vld1q (base);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */