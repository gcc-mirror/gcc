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
**	vld20.16	{q[0-9]+, q[0-9]+}, \[(?:ip|fp|r[0-9]+)\](?:	@.*|)
**	...
**	vld21.16	{q[0-9]+, q[0-9]+}, \[(?:ip|fp|r[0-9]+)\](?:	@.*|)
**	...
*/
float16x8x2_t
foo (float16_t const *addr)
{
  return vld2q_f16 (addr);
}


/*
**foo1:
**	...
**	vld20.16	{q[0-9]+, q[0-9]+}, \[(?:ip|fp|r[0-9]+)\](?:	@.*|)
**	...
**	vld21.16	{q[0-9]+, q[0-9]+}, \[(?:ip|fp|r[0-9]+)\](?:	@.*|)
**	...
*/
float16x8x2_t
foo1 (float16_t const *addr)
{
  return vld2q (addr);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */