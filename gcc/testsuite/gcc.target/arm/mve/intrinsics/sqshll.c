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
**	sqshll	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), #[0-9]+(?:	@.*|)
**	...
*/
int64_t
foo (int64_t value)
{
  return sqshll (value, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
