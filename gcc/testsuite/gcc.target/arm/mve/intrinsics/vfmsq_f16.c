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
**	vfms.f16	q[0-9]+, q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
float16x8_t
foo (float16x8_t add, float16x8_t m1, float16x8_t m2)
{
  return vfmsq_f16 (add, m1, m2);
}


/*
**foo1:
**	...
**	vfms.f16	q[0-9]+, q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
float16x8_t
foo1 (float16x8_t add, float16x8_t m1, float16x8_t m2)
{
  return vfmsq (add, m1, m2);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
