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
**	vld20.32	{q[0-9]+, q[0-9]+}, \[(?:ip|fp|r[0-9]+)\](?:	@.*|)
**	...
**	vld21.32	{q[0-9]+, q[0-9]+}, \[(?:ip|fp|r[0-9]+)\](?:	@.*|)
**	...
*/
float32x4x2_t
foo (float32_t const *addr)
{
  return vld2q_f32 (addr);
}


/*
**foo1:
**	...
**	vld20.32	{q[0-9]+, q[0-9]+}, \[(?:ip|fp|r[0-9]+)\](?:	@.*|)
**	...
**	vld21.32	{q[0-9]+, q[0-9]+}, \[(?:ip|fp|r[0-9]+)\](?:	@.*|)
**	...
*/
float32x4x2_t
foo1 (float32_t const *addr)
{
  return vld2q (addr);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */