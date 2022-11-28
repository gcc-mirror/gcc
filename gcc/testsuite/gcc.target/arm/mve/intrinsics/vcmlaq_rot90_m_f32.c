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
**	vcmlat.f32	q[0-9]+, q[0-9]+, q[0-9]+, #90(?:	@.*|)
**	...
*/
float32x4_t
foo (float32x4_t a, float32x4_t b, float32x4_t c, mve_pred16_t p)
{
  return vcmlaq_rot90_m_f32 (a, b, c, p);
}


/*
**foo1:
**	...
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vcmlat.f32	q[0-9]+, q[0-9]+, q[0-9]+, #90(?:	@.*|)
**	...
*/
float32x4_t
foo1 (float32x4_t a, float32x4_t b, float32x4_t c, mve_pred16_t p)
{
  return vcmlaq_rot90_m (a, b, c, p);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */