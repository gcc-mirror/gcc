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
**	vcmp.i16	eq, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vmrs	(?:ip|fp|r[0-9]+), p0(?:	@.*|)
**	...
*/
mve_pred16_t
foo (uint16x8_t a, uint16_t b)
{
  return vcmpeqq_n_u16 (a, b);
}


/*
**foo1:
**	...
**	vcmp.i16	eq, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vmrs	(?:ip|fp|r[0-9]+), p0(?:	@.*|)
**	...
*/
mve_pred16_t
foo1 (uint16x8_t a, uint16_t b)
{
  return vcmpeqq (a, b);
}

/*
**foo2: { xfail *-*-* }
**	...
**	vcmp.i16	eq, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vmrs	(?:ip|fp|r[0-9]+), p0(?:	@.*|)
**	...
*/
mve_pred16_t
foo2 (uint16x8_t a)
{
  return vcmpeqq (a, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */