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
**	vmov	d[0-9]+, (?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint64x2_t
foo (uint64_t a, uint64x2_t b)
{
  return vsetq_lane_u64 (a, b, 1);
}


/*
**foo1:
**	...
**	vmov	d[0-9]+, (?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint64x2_t
foo1 (uint64_t a, uint64x2_t b)
{
  return vsetq_lane (a, b, 1);
}

/*
**foo2:
**	...
**	vmov	d[0-9]+, (?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint64x2_t
foo2 (uint64x2_t b)
{
  return vsetq_lane (1, b, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */