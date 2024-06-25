/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

  /* Test with a constant that fits in vmov.  */
/*
**foo1:
**	...
**	vmov.f32	q[0-9]+, #0.0  .*
**	...
*/
float32x4_t
foo1 ()
{
  return vdupq_n_f32 (0);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
