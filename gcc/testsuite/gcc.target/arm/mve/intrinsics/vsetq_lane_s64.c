/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_mve.h"

/*
**foo:
**	...
**	vmov	d[0-9]+, (?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
int64x2_t
foo (int64_t a, int64x2_t b)
{
  return vsetq_lane_s64 (a, b, 1);
}


/*
**foo1:
**	...
**	vmov	d[0-9]+, (?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
int64x2_t
foo1 (int64_t a, int64x2_t b)
{
  return vsetq_lane (a, b, 1);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */