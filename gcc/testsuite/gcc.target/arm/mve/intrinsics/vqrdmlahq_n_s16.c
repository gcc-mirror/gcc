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
**	vqrdmlah.s16	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
int16x8_t
foo (int16x8_t add, int16x8_t m1, int16_t m2)
{
  return vqrdmlahq_n_s16 (add, m1, m2);
}


/*
**foo1:
**	...
**	vqrdmlah.s16	q[0-9]+, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
int16x8_t
foo1 (int16x8_t add, int16x8_t m1, int16_t m2)
{
  return vqrdmlahq (add, m1, m2);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
