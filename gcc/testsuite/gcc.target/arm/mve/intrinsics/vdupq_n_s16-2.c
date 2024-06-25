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
**	mov	r[0-9]+, #1000(?:	@.*|)
**	vdup.16	q[0-9]+, r[0-9]+
**	...
*/
int16x8_t
foo1 ()
{
  return vdupq_n_s16 (1000);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
