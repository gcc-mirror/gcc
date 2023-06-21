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
**	vldrb.s16	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+\](?:	@.*|)
**	...
*/
int16x8_t
foo (int8_t const *base, uint16x8_t offset)
{
  return vldrbq_gather_offset_s16 (base, offset);
}


/*
**foo1:
**	...
**	vldrb.s16	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+\](?:	@.*|)
**	...
*/
int16x8_t
foo1 (int8_t const *base, uint16x8_t offset)
{
  return vldrbq_gather_offset (base, offset);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */