/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

  /* Test with a constant that does not fit in vmov.  */
/*
**foo1:
**	...
**	mov	r[0-9]+, #1000
**	vdup.32	q[0-9]+, r[0-9]+
**	...
*/
int32x4_t
foo1 ()
{
  return vdupq_n_s32 (1000);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
