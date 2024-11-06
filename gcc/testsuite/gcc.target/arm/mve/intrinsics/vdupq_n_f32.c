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
**	vdup.32	q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
float32x4_t
foo (float32_t a)
{
  return vdupq_n_f32 (a);
}

/*
**foo1:
**	...
**	ldr	r[0-9]+, .L.*
**	vdup.32	q[0-9]+, r[0-9]+
**	...
*/
float32x4_t
foo1 ()
{
  return vdupq_n_f32 (1.1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
