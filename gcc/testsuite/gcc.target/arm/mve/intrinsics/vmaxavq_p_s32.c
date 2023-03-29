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
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vmaxavt.s32	(?:ip|fp|r[0-9]+), q[0-9]+(?:	@.*|)
**	...
*/
uint32_t
foo (uint32_t a, int32x4_t b, mve_pred16_t p)
{
  return vmaxavq_p_s32 (a, b, p);
}


/*
**foo1:
**	...
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vmaxavt.s32	(?:ip|fp|r[0-9]+), q[0-9]+(?:	@.*|)
**	...
*/
uint32_t
foo1 (uint32_t a, int32x4_t b, mve_pred16_t p)
{
  return vmaxavq_p (a, b, p);
}

/*
**foo2:
**	...
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vmaxavt.s32	(?:ip|fp|r[0-9]+), q[0-9]+(?:	@.*|)
**	...
*/
uint32_t
foo2 (int32x4_t b, mve_pred16_t p)
{
  return vmaxavq_p (1, b, p);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */