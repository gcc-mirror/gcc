/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
**foo:
**	...
**	vdup.16	q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint16x8_t
foo (uint16_t a)
{
  return vdupq_n_u16 (a);
}

/*
**foo1:
**	...
**	vmov.i16	q[0-9]+, (#0x1)  (?:@.*|)
**	...
*/
uint16x8_t
foo1 ()
{
  return vdupq_n_u16 (1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
