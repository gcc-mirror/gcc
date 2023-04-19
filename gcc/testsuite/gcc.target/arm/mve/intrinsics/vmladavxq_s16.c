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
**	vmladavx.s16	(?:ip|fp|r[0-9]+), q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
int32_t
foo (int16x8_t m1, int16x8_t m2)
{
  return vmladavxq_s16 (m1, m2);
}


/*
**foo1:
**	...
**	vmladavx.s16	(?:ip|fp|r[0-9]+), q[0-9]+, q[0-9]+(?:	@.*|)
**	...
*/
int32_t
foo1 (int16x8_t m1, int16x8_t m2)
{
  return vmladavxq (m1, m2);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
