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
**	vcadd.i16	q[0-9]+, q[0-9]+, q[0-9]+, #90(?:	@.*|)
**	...
*/
uint16x8_t
foo (uint16x8_t a, uint16x8_t b)
{
  return vcaddq_rot90_u16 (a, b);
}


/*
**foo1:
**	...
**	vcadd.i16	q[0-9]+, q[0-9]+, q[0-9]+, #90(?:	@.*|)
**	...
*/
uint16x8_t
foo1 (uint16x8_t a, uint16x8_t b)
{
  return vcaddq_rot90 (a, b);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */