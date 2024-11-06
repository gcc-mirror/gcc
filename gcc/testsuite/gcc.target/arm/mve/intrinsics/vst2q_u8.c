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
**	vst20.8	{q[0-9]+, q[0-9]+}, \[(?:ip|fp|r[0-9]+)\](?:	@.*|)
**	...
**	vst21.8	{q[0-9]+, q[0-9]+}, \[(?:ip|fp|r[0-9]+)\](?:	@.*|)
**	...
*/
void
foo (uint8_t *addr, uint8x16x2_t value)
{
  vst2q_u8 (addr, value);
}


/*
**foo1:
**	...
**	vst20.8	{q[0-9]+, q[0-9]+}, \[(?:ip|fp|r[0-9]+)\](?:	@.*|)
**	...
**	vst21.8	{q[0-9]+, q[0-9]+}, \[(?:ip|fp|r[0-9]+)\](?:	@.*|)
**	...
*/
void
foo1 (uint8_t *addr, uint8x16x2_t value)
{
  vst2q (addr, value);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
