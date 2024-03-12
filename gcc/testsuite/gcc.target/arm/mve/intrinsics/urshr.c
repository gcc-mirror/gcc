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
**	urshr	(?:ip|fp|r[0-9]+), #1(?:	@.*|)
**	...
*/
uint32_t
foo (uint32_t value)
{
  return urshr (value, 1);
}

/*
**foo1:
**	...
**	urshr	(?:ip|fp|r[0-9]+), #1(?:	@.*|)
**	...
*/
uint32_t
foo1 ()
{
  return urshr (1, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
