 /* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Check that calling lsll with an out of range shift amount is not interpreted
   as undefined behavior, and that we actually use the lsll instruction (except
   if a negative shift amount can be handled by asrl).  Check code generation
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

     lsll r0,r1,#3  */
/*
**foo_3:
**	...
**	lsll	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), #3(?:	@.*|)
**	...
*/
uint64_t
foo_3 (uint64_t value)
{
  return lsll (value, 3);
}

  /* Negative shift amount in [-32.-1] range, reverse shift (asrl) with the
     opposite shift amount as immediate:

     lsrl r0, r1, #3  */
/*
**foo_m3:
**	...
**	lsrl	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), #3(?:	@.*|)
**	...
*/
uint64_t
foo_m3 (uint64_t value)
{
  return lsll (value, -3);
}

  /* Out of [1..32] range positive shift amount, but < 64.
     high_out = low_in << (amount - 32) (using lsls, not lsll)
     low_out = 0

     lsls r1,r0,#1
     movs r0, #0  */
/*
**foo_33:
**	...
**	lsls	(?:ip|fp|r[0-9]+), (ip|fp|r[0-9]+), #1(?:	@.*|)
**	movs	\1, #0(?:	@.*|)
**	...
*/
uint64_t
foo_33 (uint64_t value)
{
  return lsll (value, 33);
}

  /* Out of [-32..-1] range negative shift amount, but > -64.  Reverse shift
     (lsrl equivalent) in [33..64] range:
     lo_out = hi_in >> (amount - 32)
     hi_out = 0

     lsrs r0, r1, #1
     movs r1, #0  */
/*
**foo_m33:
**	...
**	lsrs	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), #1(?:	@.*|)
**	movs	(?:ip|fp|r[0-9]+), #0(?:	@.*|)
**	...
*/
uint64_t
foo_m33 (uint64_t value)
{
  return lsll (value, -33);
}

  /* Out of range positive shift amount (>= 64), result is 0.

     movs r0, #0
     movs r1, #0  */
/*
**foo_65:
**	...
**	movs	(ip|fp|r[0-9]+), #0(?:	@.*|)
**	movs	(ip|fp|r[0-9]+), #0(?:	@.*|)
**	...
*/
uint64_t
foo_65 (uint64_t value)
{
  return lsll (value, 65);
}

  /* Out of range negative shift amount (<= -64), result is 0, because lsll
     uses an unsigned input.

     movs r0, #0
     movs r1, #0  */
/*
**foo_m65:
**	...
**	movs	(ip|fp|r[0-9]+), #0(?:	@.*|)
**	movs	(ip|fp|r[0-9]+), #0(?:	@.*|)
**	...
*/
uint64_t
foo_m65 (uint64_t value)
{
  return lsll (value, -65);
}

  /* shift amount == 0, use a mov, which is optimized out.  */
/*
**foo_0:
**	bx	lr
**	...
*/
uint64_t
foo_0 (uint64_t value)
{
  return lsll (value, 0);
}

  /* Unknown shift amount, use the register variant.

   lsll r0, r1, r2  */
/*
**foo_var:
**	...
**	lsll	(?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+), (?:ip|fp|r[0-9]+)(?:	@.*|)
**	...
*/
uint64_t
foo_var (uint64_t value, int32_t amount)
{
  return lsll (value, amount);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
