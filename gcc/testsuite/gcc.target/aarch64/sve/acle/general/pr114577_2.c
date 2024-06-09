/* { dg-options "-O -msve-vector-bits=256" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_neon_sve_bridge.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
** f1:		{ target aarch64_little_endian }
**	ldr	q0, \[x0\]
**	ret
*/
svint32_t
f1 (int *a)
{
  return svset_neonq (svundef_s32 (), vld1q_s32 (a));
}

/*
** f2:		{ target aarch64_little_endian }
**	ldr	q0, \[x0\]
**	ret
*/
svint32_t
f2 (int *a)
{
  svint32_t undef;
  return svset_neonq (undef, vld1q_s32 (a));
}

/*
** f3:		{ target aarch64_little_endian }
**	mov	[vz]0.[^\n]+, [vz]1.[^\n]+
**	ret
*/
svint32_t
f3 (int32x4_t v0, int32x4_t v1)
{
  return svset_neonq (svundef_s32 (), v1);
}

#ifdef __cplusplus
}
#endif
