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
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vcaddt.i16	q[0-9]+, q[0-9]+, q[0-9]+, #90(?:	@.*|)
**	...
*/
int16x8_t
foo (int16x8_t a, int16x8_t b, mve_pred16_t p)
{
  return vcaddq_rot90_x_s16 (a, b, p);
}


/*
**foo1:
**	...
**	vmsr	p0, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vpst(?:	@.*|)
**	...
**	vcaddt.i16	q[0-9]+, q[0-9]+, q[0-9]+, #90(?:	@.*|)
**	...
*/
int16x8_t
foo1 (int16x8_t a, int16x8_t b, mve_pred16_t p)
{
  return vcaddq_rot90_x (a, b, p);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */