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
**	vaddlva.u32	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), q[0-9]+(?:	@.*|)
**	...
*/
uint64_t
foo (uint64_t a, uint32x4_t b)
{
  return vaddlvaq_u32 (a, b);
}


/*
**foo1:
**	...
**	vaddlva.u32	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), q[0-9]+(?:	@.*|)
**	...
*/
uint64_t
foo1 (uint64_t a, uint32x4_t b)
{
  return vaddlvaq (a, b);
}

/*
**foo2:
**	...
**	vaddlva.u32	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), q[0-9]+(?:	@.*|)
**	...
*/
uint64_t
foo2 (uint32x4_t b)
{
  return vaddlvaq (1, b);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */