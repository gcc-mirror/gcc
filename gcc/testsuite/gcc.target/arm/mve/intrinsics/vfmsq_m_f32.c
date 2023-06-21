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
**	vfmst.f32	q[0-9]+, q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
float32x4_t
foo (float32x4_t add, float32x4_t m1, float32x4_t m2, mve_pred16_t p)
{
  return vfmsq_m_f32 (add, m1, m2, p);
}


/*
**foo1:
**	...
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vfmst.f32	q[0-9]+, q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
float32x4_t
foo1 (float32x4_t add, float32x4_t m1, float32x4_t m2, mve_pred16_t p)
{
  return vfmsq_m (add, m1, m2, p);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
