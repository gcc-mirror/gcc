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
**	uqrshl	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint32_t
foo (uint32_t value, int32_t shift)
{
  return uqrshl (value, shift);
}

/*
**foo1:
**	...
**	uqrshl	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint32_t
foo1 (int32_t shift)
{
  return uqrshl (1, shift);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
