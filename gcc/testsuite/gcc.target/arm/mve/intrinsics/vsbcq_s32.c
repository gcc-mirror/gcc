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
**	vmrs	(?:ip|fp|r[0-9]+), FPSCR_nzcvqc(?:	@.*|)
**	...
**	bfi	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), #29, #1(?:	@.*|)
**	...
**	vmsr	FPSCR_nzcvqc, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vsbc.i32	q[0-9]+, q[0-9]+, q[0-9]+(?:	@.*|)
**	...
**	vmrs	(?:ip|fp|r[0-9]+), FPSCR_nzcvqc(?:	@.*|)
**	...
**	ubfx	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), #29, #1(?:	@.*|)
**	...
*/
int32x4_t
foo (int32x4_t a, int32x4_t b, unsigned *carry)
{
  return vsbcq_s32 (a, b, carry);
}


/*
**foo1:
**	...
**	vmrs	(?:ip|fp|r[0-9]+), FPSCR_nzcvqc(?:	@.*|)
**	...
**	bfi	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), #29, #1(?:	@.*|)
**	...
**	vmsr	FPSCR_nzcvqc, (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
**	vsbc.i32	q[0-9]+, q[0-9]+, q[0-9]+(?:	@.*|)
**	...
**	vmrs	(?:ip|fp|r[0-9]+), FPSCR_nzcvqc(?:	@.*|)
**	...
**	ubfx	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), #29, #1(?:	@.*|)
**	...
*/
int32x4_t
foo1 (int32x4_t a, int32x4_t b, unsigned *carry)
{
  return vsbcq (a, b, carry);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
