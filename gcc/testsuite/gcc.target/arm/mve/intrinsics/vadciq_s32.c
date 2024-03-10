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
**	vadci.i32	q[0-9]+, q[0-9]+, q[0-9]+(?:	@.*|)
**	...
**	vmrs	(?:ip|fp|r[0-9]+), FPSCR_nzcvqc(?:	@.*|)
**	...
**	ubfx	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), #29, #1(?:	@.*|)
**	...
*/
int32x4_t
foo (int32x4_t a, int32x4_t b, unsigned *carry_out)
{
  return vadciq_s32 (a, b, carry_out);
}


/*
**foo1:
**	...
**	vadci.i32	q[0-9]+, q[0-9]+, q[0-9]+(?:	@.*|)
**	...
**	vmrs	(?:ip|fp|r[0-9]+), FPSCR_nzcvqc(?:	@.*|)
**	...
**	ubfx	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), #29, #1(?:	@.*|)
**	...
*/
int32x4_t
foo1 (int32x4_t a, int32x4_t b, unsigned *carry_out)
{
  return vadciq (a, b, carry_out);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
