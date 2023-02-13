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
**	vcaddt.f32	q[0-9]+, q[0-9]+, q[0-9]+, #90(?:	@.*|)
**	...
*/
float32x4_t
foo (float32x4_t a, float32x4_t b, mve_pred16_t p)
{
  return vcaddq_rot90_x_f32 (a, b, p);
}


/*
**foo1:
**	...
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vcaddt.f32	q[0-9]+, q[0-9]+, q[0-9]+, #90(?:	@.*|)
**	...
*/
float32x4_t
foo1 (float32x4_t a, float32x4_t b, mve_pred16_t p)
{
  return vcaddq_rot90_x (a, b, p);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */