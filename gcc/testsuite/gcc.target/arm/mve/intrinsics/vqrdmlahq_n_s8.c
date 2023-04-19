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
**	vqrdmlah.s8	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
int8x16_t
foo (int8x16_t add, int8x16_t m1, int8_t m2)
{
  return vqrdmlahq_n_s8 (add, m1, m2);
}


/*
**foo1:
**	...
**	vqrdmlah.s8	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
int8x16_t
foo1 (int8x16_t add, int8x16_t m1, int8_t m2)
{
  return vqrdmlahq (add, m1, m2);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
