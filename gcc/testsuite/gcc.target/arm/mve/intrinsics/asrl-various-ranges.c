/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Check that calling asrl with an out of range shift amount is not interpreted
   as undefined behavior, and that we actually use the asrl instruction (except
   if a negative shift amount can be handled by lsll).  Check code generation
   for various special cases:
   1 <= amount <= 32
   -32 <= amount <= -1
   32 < amount < 64
   -64 < amount < -32
   amount >= 64
   amount <= -64
   amount == 0
   amount unknown at compile time. */
#include "arm_mve.h"

#ifdef __cplusplus
extern "C" {
#endif

  /* Positive shift amount in [1.32] range, use the immediate:

   asrl r0, r1, #3  */
/*
**foo_3:
**	...
**	asrl	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), #3(?:	@.*|)
**	...
*/
int64_t
foo_3 (int64_t value)
{
  return asrl (value, 3);
}

  /* Negative shift amount in [-32.-1] range, reverse shift (lsll) with the
     opposite shift amount as immediate:

     lsll r0, r1, #3  */
/*
**foo_m3:
**	...
**	lsll	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), #3(?:	@.*|)
**	...
*/
int64_t
foo_m3 (int64_t value)
{
  return asrl (value, -3);
}

  /* Out of [1.32] range positive shift amount, but < 64.
     lo_out = hi_in >> (amount - 32)
     hi_out = hi_in >> 31 (to copy the sign bit)

     asrs r0, r1, #1
     asrs r1, r1, #31   */
/*
**foo_33:
**	...
**	asrs	(?:ip|fp|r[0-9]+), (ip|fp|r[0-9]+), #1(?:	@.*|)
**	asrs	(?:ip|fp|r[0-9]+), \1, #31(?:	@.*|)
**	...
*/
int64_t
foo_33 (int64_t value)
{
  return asrl (value, 33);
}

  /* Out of [-32..-1] range negative shift amount, but > -64. Reverse shift
     (lsll equivalent) in [33.64] range:
     hi_out = lo_in << (-amount - 32)
     lo_out = 0 

     lsls r1, r0, #1
     movs r0, #0  */
/*
**foo_m33:
**	...
**	lsls	(?:ip|fp|r[0-9]+), (ip|fp|r[0-9]+), #1(?:	@.*|)
**	movs	\1, #0(?:	@.*|)
**	...
*/
int64_t
foo_m33 (int64_t value)
{
  return asrl (value, -33);
}

  /* Out of range positive shift amount (>= 64)
     lo_out = hi_in >> 31 (copy sign bit)
     hi_out = hi_in >> 31

     asrs r0, r1, #31
     mov r1, r0  */
/*
**foo_65:
**	...
**	asrs	(ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), #31(?:	@.*|)
**	mov	(?:ip|fp|r[0-9]+), \1(?:	@.*|)
**	...
*/
int64_t
foo_65 (int64_t value)
{
  return asrl (value, 65);
}

  /* Out of range negative shift amount (<= 64), result is 0.

   movs r0, #0
   movs r1, #0  */
/*
**foo_m65:
**	...
**	movs	(ip|fp|r[0-9]+), #0(?:	@.*|)
**	movs	(ip|fp|r[0-9]+), #0(?:	@.*|)
**	...
*/
int64_t
foo_m65 (int64_t value)
{
  return asrl (value, -65);
}

  /* shift amount == 0, use a mov, which is optimized out.  */
/*
**foo_0:
**	bx	lr
**	...
*/
int64_t
foo_0 (int64_t value)
{
  return asrl (value, 0);
}

  /* Unknown shift amount, use the register variant.

     asrl r0, r1, r2  */
/*
**foo_var:
**	...
**	asrl	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
int64_t
foo_var (int64_t value, int32_t amount)
{
  return asrl (value, amount);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
