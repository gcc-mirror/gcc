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
**	vst40.32	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vst41.32	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vst42.32	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vst43.32	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	...
*/
void
foo (uint32_t *addr, uint32x4x4_t value)
{
  return vst4q_u32 (addr, value);
}


/*
**foo1:
**	...
**	vst40.32	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vst41.32	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vst42.32	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	vst43.32	{q[0-9+], q[0-9+], q[0-9+], q[0-9+]}, \[r[0-9+]\]
**	...
*/
void
foo1 (uint32_t *addr, uint32x4x4_t value)
{
  return vst4q (addr, value);
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
