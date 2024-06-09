/* { dg-options "-O" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_neon_sve_bridge.h>

#ifdef __cplusplus
extern "C" {
#endif

svint32_t svundef_foo ();

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

/*
** f4:		{ target aarch64_little_endian }
**	uzp1	z([0-9]+)\.s, z0\.s, z1\.s
**	ldr	q([0-9]+), \[x0\]
**	ptrue	p([0-7])\.s, vl4
**	sel	z0\.s, p\3, z\2\.s, z\1\.s
**	ret
*/
svint32_t
f4 (int *a, svint32_t x, svint32_t y)
{
  x = svuzp1 (x, y);
  int32x4_t z = vld1q_s32 (a);
  return svset_neonq (x, z);
}

/*
** f5:
**	...
**	bl	svundef_foo
**	...
**	sel	z0\.s, [^\n]+
**	...
**	ret
*/
svint32_t
f5 (int *a)
{
  return svset_neonq (svundef_foo (), vld1q_s32 (a));
}

/*
** f6:
**	...
**	blr	x[0-9]+
**	...
**	sel	z0\.s, [^\n]+
**	...
**	ret
*/
svint32_t
f6 (int *a, svint32_t (*svundef_s32) ())
{
  return svset_neonq (svundef_s32 (), vld1q_s32 (a));
}

#ifdef __cplusplus
}
#endif
