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
**	vmlas.s8	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
int8x16_t
foo (int8x16_t m1, int8x16_t m2, int8_t add)
{
  return vmlasq_n_s8 (m1, m2, add);
}


/*
**foo1:
**	...
**	vmlas.s8	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
int8x16_t
foo1 (int8x16_t m1, int8x16_t m2, int8_t add)
{
  return vmlasq (m1, m2, add);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */