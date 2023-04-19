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
**	vcvt.f32.u32	q[0-9]+, q[0-9]+, #[0-9]+(?:	@.*|)
**	...
*/
float32x4_t
foo (uint32x4_t a)
{
  return vcvtq_n_f32_u32 (a, 1);
}


/*
**foo1:
**	...
**	vcvt.f32.u32	q[0-9]+, q[0-9]+, #[0-9]+(?:	@.*|)
**	...
*/
float32x4_t
foo1 (uint32x4_t a)
{
  return vcvtq_n (a, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
