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
**	vst40.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vst41.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vst42.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vst43.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	...
*/
void
foo (int16_t *addr, int16x8x4_t value)
{
  vst4q_s16 (addr, value);
}


/*
**foo1:
**	...
**	vst40.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vst41.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vst42.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vst43.16	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	...
*/
void
foo1 (int16_t *addr, int16x8x4_t value)
{
  vst4q (addr, value);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
