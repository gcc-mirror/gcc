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
**	vld40.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vld41.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vld42.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vld43.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	...
*/
int16x8x4_t
foo (int16_t const *addr)
{
  return vld4q_s16 (addr);
}


/*
**foo1:
**	...
**	vld40.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vld41.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vld42.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vld43.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	...
*/
int16x8x4_t
foo1 (int16_t const *addr)
{
  return vld4q (addr);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */