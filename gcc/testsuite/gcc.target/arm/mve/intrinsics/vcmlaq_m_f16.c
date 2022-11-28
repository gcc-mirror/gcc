/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
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
**	vcmlat.f16	q[0-9]+, q[0-9]+, q[0-9]+, #0(?:	@.*|)
**	...
*/
float16x8_t
foo (float16x8_t a, float16x8_t b, float16x8_t c, mve_pred16_t p)
{
  return vcmlaq_m_f16 (a, b, c, p);
}


/*
**foo1:
**	...
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vcmlat.f16	q[0-9]+, q[0-9]+, q[0-9]+, #0(?:	@.*|)
**	...
*/
float16x8_t
foo1 (float16x8_t a, float16x8_t b, float16x8_t c, mve_pred16_t p)
{
  return vcmlaq_m (a, b, c, p);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */