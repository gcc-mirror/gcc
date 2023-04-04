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
**	vmov q[0-9+]\[2\], q[0-9+]\[0\], r0, r2
**	vmov q[0-9+]\[3\], q[0-9+]\[1\], r1, r3
**	...
*/
int16x8_t
foo (uint64_t a, uint64_t b)
{
  return vcreateq_s16 (a, b);
}

/*
**foo1:
**	...
**	vmov q[0-9+]\[2\], q[0-9+]\[0\], r[0-9+], r[0-9+]
**	vmov q[0-9+]\[3\], q[0-9+]\[1\], r[0-9+], r[0-9+]
**	...
*/
int16x8_t
foo1 ()
{
  return vcreateq_s16 (1, 1);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
