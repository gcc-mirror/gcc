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
**	vrmlaldavha.u32	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
uint64_t
foo (uint64_t a, uint32x4_t b, uint32x4_t c)
{
  return vrmlaldavhaq_u32 (a, b, c);
}


/*
**foo1:
**	...
**	vrmlaldavha.u32	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
uint64_t
foo1 (uint64_t a, uint32x4_t b, uint32x4_t c)
{
  return vrmlaldavhaq (a, b, c);
}

/*
**foo2:
**	...
**	vrmlaldavha.u32	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
uint64_t
foo2 (uint32x4_t b, uint32x4_t c)
{
  return vrmlaldavhaq (1, b, c);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
