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
**	vldrd.u64	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+, uxtw #3\](?:	@.*|)
**	...
*/
int64x2_t
foo (int64_t const *base, uint64x2_t offset)
{
  return vldrdq_gather_shifted_offset_s64 (base, offset);
}


/*
**foo1:
**	...
**	vldrd.u64	q[0-9]+, \[(?:ip|fp|r[0-9]+), q[0-9]+, uxtw #3\](?:	@.*|)
**	...
*/
int64x2_t
foo1 (int64_t const *base, uint64x2_t offset)
{
  return vldrdq_gather_shifted_offset (base, offset);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */