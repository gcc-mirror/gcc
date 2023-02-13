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
**	vcadd.i32	q[0-9]+, q[0-9]+, q[0-9]+, #270(?:	@.*|)
**	...
*/
uint32x4_t
foo (uint32x4_t a, uint32x4_t b)
{
  return vcaddq_rot270_u32 (a, b);
}


/*
**foo1:
**	...
**	vcadd.i32	q[0-9]+, q[0-9]+, q[0-9]+, #270(?:	@.*|)
**	...
*/
uint32x4_t
foo1 (uint32x4_t a, uint32x4_t b)
{
  return vcaddq_rot270 (a, b);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */