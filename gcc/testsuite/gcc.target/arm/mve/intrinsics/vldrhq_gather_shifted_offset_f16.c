/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
**foo:
**	...
**	vldrh.f16	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+, uxtw #1\](?:	@.*|)
**	...
*/
float16x8_t
foo (float16_t const *base, uint16x8_t offset)
{
  return vldrhq_gather_shifted_offset_f16 (base, offset);
}


/*
**foo1:
**	...
**	vldrh.f16	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+, uxtw #1\](?:	@.*|)
**	...
*/
float16x8_t
foo1 (float16_t const *base, uint16x8_t offset)
{
  return vldrhq_gather_shifted_offset (base, offset);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */