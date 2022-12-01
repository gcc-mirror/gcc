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
**	vsub.f16	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
float16x8_t
foo (float16x8_t a, float16_t b)
{
  return vsubq_n_f16 (a, b);
}


/*
**foo1:
**	...
**	vsub.f16	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
float16x8_t
foo1 (float16x8_t a, float16_t b)
{
  return vsubq (a, b);
}

/*
**foo2:
**	...
**	vsub.f16	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
float16x8_t
foo2 (float16x8_t a)
{
  return vsubq (a, 1.1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */