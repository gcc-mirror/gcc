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
**	vabav.u32	(?:ip|fp|r[0-9]+), q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
uint32_t
foo (uint32_t a, uint32x4_t b, uint32x4_t c)
{
  return vabavq_u32 (a, b, c);
}


/*
**foo1:
**	...
**	vabav.u32	(?:ip|fp|r[0-9]+), q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
uint32_t
foo1 (uint32_t a, uint32x4_t b, uint32x4_t c)
{
  return vabavq (a, b, c);
}

/*
**foo2:
**	...
**	vabav.u32	(?:ip|fp|r[0-9]+), q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
uint32_t
foo2 (uint32x4_t b, uint32x4_t c)
{
  return vabavq (1, b, c);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */