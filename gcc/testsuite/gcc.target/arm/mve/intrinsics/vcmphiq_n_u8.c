/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_mve.h"

/*
**foo:
**	...
**	vcmp.u8	hi, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vmrs	(?:ip|fp|r[0-9]+), p0(?:	@.*|)
**	...
*/
mve_pred16_t
foo (uint8x16_t a, uint8_t b)
{
  return vcmphiq_n_u8 (a, b);
}


/*
**foo1:
**	...
**	vcmp.u8	hi, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vmrs	(?:ip|fp|r[0-9]+), p0(?:	@.*|)
**	...
*/
mve_pred16_t
foo1 (uint8x16_t a, uint8_t b)
{
  return vcmphiq (a, b);
}

/*
**foo2:
**	...
**	vcmp.u8	hi, q[0-9]+, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vmrs	(?:ip|fp|r[0-9]+), p0(?:	@.*|)
**	...
*/
mve_pred16_t
foo2 (uint8x16_t a)
{
  return vcmphiq (a, 1);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */