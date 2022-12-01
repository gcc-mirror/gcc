/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_mve.h"

/*
**foo:
**	...
**	vmov q[0-9+]\[2\], q[0-9+]\[0\], r[0-9+], r[0-9+]
**	vmov q[0-9+]\[3\], q[0-9+]\[1\], r[0-9+], r[0-9+]
**	...
*/
uint8x16_t
foo (uint64_t a, uint64_t b)
{
  return vcreateq_u8 (a, b);
}

/*
**foo1:
**	...
**	vmov q[0-9+]\[2\], q[0-9+]\[0\], r[0-9+], r[0-9+]
**	vmov q[0-9+]\[3\], q[0-9+]\[1\], r[0-9+], r[0-9+]
**	...
*/
uint8x16_t
foo1 ()
{
  return vcreateq_u8 (1, 1);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */