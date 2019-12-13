/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmla_0_f32_m_tied1:
**	fcmla	z0\.s, p0/m, z1\.s, z2\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f32_m_tied1, svfloat32_t,
		z0 = svcmla_f32_m (p0, z0, z1, z2, 0),
		z0 = svcmla_m (p0, z0, z1, z2, 0))

/*
** cmla_0_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, \1\.s, z2\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f32_m_tied2, svfloat32_t,
		z0 = svcmla_f32_m (p0, z1, z0, z2, 0),
		z0 = svcmla_m (p0, z1, z0, z2, 0))

/*
** cmla_0_f32_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, \1\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f32_m_tied3, svfloat32_t,
		z0 = svcmla_f32_m (p0, z1, z2, z0, 0),
		z0 = svcmla_m (p0, z1, z2, z0, 0))

/*
** cmla_0_f32_m_untied:
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, z3\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f32_m_untied, svfloat32_t,
		z0 = svcmla_f32_m (p0, z1, z2, z3, 0),
		z0 = svcmla_m (p0, z1, z2, z3, 0))

/*
** cmla_90_f32_m_tied1:
**	fcmla	z0\.s, p0/m, z1\.s, z2\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f32_m_tied1, svfloat32_t,
		z0 = svcmla_f32_m (p0, z0, z1, z2, 90),
		z0 = svcmla_m (p0, z0, z1, z2, 90))

/*
** cmla_90_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, \1\.s, z2\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f32_m_tied2, svfloat32_t,
		z0 = svcmla_f32_m (p0, z1, z0, z2, 90),
		z0 = svcmla_m (p0, z1, z0, z2, 90))

/*
** cmla_90_f32_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, \1\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f32_m_tied3, svfloat32_t,
		z0 = svcmla_f32_m (p0, z1, z2, z0, 90),
		z0 = svcmla_m (p0, z1, z2, z0, 90))

/*
** cmla_90_f32_m_untied:
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, z3\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f32_m_untied, svfloat32_t,
		z0 = svcmla_f32_m (p0, z1, z2, z3, 90),
		z0 = svcmla_m (p0, z1, z2, z3, 90))

/*
** cmla_180_f32_m_tied1:
**	fcmla	z0\.s, p0/m, z1\.s, z2\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f32_m_tied1, svfloat32_t,
		z0 = svcmla_f32_m (p0, z0, z1, z2, 180),
		z0 = svcmla_m (p0, z0, z1, z2, 180))

/*
** cmla_180_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, \1\.s, z2\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f32_m_tied2, svfloat32_t,
		z0 = svcmla_f32_m (p0, z1, z0, z2, 180),
		z0 = svcmla_m (p0, z1, z0, z2, 180))

/*
** cmla_180_f32_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, \1\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f32_m_tied3, svfloat32_t,
		z0 = svcmla_f32_m (p0, z1, z2, z0, 180),
		z0 = svcmla_m (p0, z1, z2, z0, 180))

/*
** cmla_180_f32_m_untied:
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, z3\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f32_m_untied, svfloat32_t,
		z0 = svcmla_f32_m (p0, z1, z2, z3, 180),
		z0 = svcmla_m (p0, z1, z2, z3, 180))

/*
** cmla_270_f32_m_tied1:
**	fcmla	z0\.s, p0/m, z1\.s, z2\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f32_m_tied1, svfloat32_t,
		z0 = svcmla_f32_m (p0, z0, z1, z2, 270),
		z0 = svcmla_m (p0, z0, z1, z2, 270))

/*
** cmla_270_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, \1\.s, z2\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f32_m_tied2, svfloat32_t,
		z0 = svcmla_f32_m (p0, z1, z0, z2, 270),
		z0 = svcmla_m (p0, z1, z0, z2, 270))

/*
** cmla_270_f32_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, \1\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f32_m_tied3, svfloat32_t,
		z0 = svcmla_f32_m (p0, z1, z2, z0, 270),
		z0 = svcmla_m (p0, z1, z2, z0, 270))

/*
** cmla_270_f32_m_untied:
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, z3\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f32_m_untied, svfloat32_t,
		z0 = svcmla_f32_m (p0, z1, z2, z3, 270),
		z0 = svcmla_m (p0, z1, z2, z3, 270))

/*
** cmla_0_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fcmla	z0\.s, p0/m, z1\.s, z2\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f32_z_tied1, svfloat32_t,
		z0 = svcmla_f32_z (p0, z0, z1, z2, 0),
		z0 = svcmla_z (p0, z0, z1, z2, 0))

/*
** cmla_0_f32_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, z1\.s
**	fcmla	z0\.s, p0/m, \1\.s, z2\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f32_z_tied2, svfloat32_t,
		z0 = svcmla_f32_z (p0, z1, z0, z2, 0),
		z0 = svcmla_z (p0, z1, z0, z2, 0))

/*
** cmla_0_f32_z_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, z1\.s
**	fcmla	z0\.s, p0/m, z2\.s, \1\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f32_z_tied3, svfloat32_t,
		z0 = svcmla_f32_z (p0, z1, z2, z0, 0),
		z0 = svcmla_z (p0, z1, z2, z0, 0))

/*
** cmla_0_f32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	fcmla	z0\.s, p0/m, z2\.s, z3\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f32_z_untied, svfloat32_t,
		z0 = svcmla_f32_z (p0, z1, z2, z3, 0),
		z0 = svcmla_z (p0, z1, z2, z3, 0))

/*
** cmla_90_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fcmla	z0\.s, p0/m, z1\.s, z2\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f32_z_tied1, svfloat32_t,
		z0 = svcmla_f32_z (p0, z0, z1, z2, 90),
		z0 = svcmla_z (p0, z0, z1, z2, 90))

/*
** cmla_90_f32_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, z1\.s
**	fcmla	z0\.s, p0/m, \1\.s, z2\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f32_z_tied2, svfloat32_t,
		z0 = svcmla_f32_z (p0, z1, z0, z2, 90),
		z0 = svcmla_z (p0, z1, z0, z2, 90))

/*
** cmla_90_f32_z_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, z1\.s
**	fcmla	z0\.s, p0/m, z2\.s, \1\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f32_z_tied3, svfloat32_t,
		z0 = svcmla_f32_z (p0, z1, z2, z0, 90),
		z0 = svcmla_z (p0, z1, z2, z0, 90))

/*
** cmla_90_f32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	fcmla	z0\.s, p0/m, z2\.s, z3\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f32_z_untied, svfloat32_t,
		z0 = svcmla_f32_z (p0, z1, z2, z3, 90),
		z0 = svcmla_z (p0, z1, z2, z3, 90))

/*
** cmla_180_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fcmla	z0\.s, p0/m, z1\.s, z2\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f32_z_tied1, svfloat32_t,
		z0 = svcmla_f32_z (p0, z0, z1, z2, 180),
		z0 = svcmla_z (p0, z0, z1, z2, 180))

/*
** cmla_180_f32_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, z1\.s
**	fcmla	z0\.s, p0/m, \1\.s, z2\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f32_z_tied2, svfloat32_t,
		z0 = svcmla_f32_z (p0, z1, z0, z2, 180),
		z0 = svcmla_z (p0, z1, z0, z2, 180))

/*
** cmla_180_f32_z_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, z1\.s
**	fcmla	z0\.s, p0/m, z2\.s, \1\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f32_z_tied3, svfloat32_t,
		z0 = svcmla_f32_z (p0, z1, z2, z0, 180),
		z0 = svcmla_z (p0, z1, z2, z0, 180))

/*
** cmla_180_f32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	fcmla	z0\.s, p0/m, z2\.s, z3\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f32_z_untied, svfloat32_t,
		z0 = svcmla_f32_z (p0, z1, z2, z3, 180),
		z0 = svcmla_z (p0, z1, z2, z3, 180))

/*
** cmla_270_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fcmla	z0\.s, p0/m, z1\.s, z2\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f32_z_tied1, svfloat32_t,
		z0 = svcmla_f32_z (p0, z0, z1, z2, 270),
		z0 = svcmla_z (p0, z0, z1, z2, 270))

/*
** cmla_270_f32_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, z1\.s
**	fcmla	z0\.s, p0/m, \1\.s, z2\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f32_z_tied2, svfloat32_t,
		z0 = svcmla_f32_z (p0, z1, z0, z2, 270),
		z0 = svcmla_z (p0, z1, z0, z2, 270))

/*
** cmla_270_f32_z_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, z1\.s
**	fcmla	z0\.s, p0/m, z2\.s, \1\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f32_z_tied3, svfloat32_t,
		z0 = svcmla_f32_z (p0, z1, z2, z0, 270),
		z0 = svcmla_z (p0, z1, z2, z0, 270))

/*
** cmla_270_f32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	fcmla	z0\.s, p0/m, z2\.s, z3\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f32_z_untied, svfloat32_t,
		z0 = svcmla_f32_z (p0, z1, z2, z3, 270),
		z0 = svcmla_z (p0, z1, z2, z3, 270))

/*
** cmla_0_f32_x_tied1:
**	fcmla	z0\.s, p0/m, z1\.s, z2\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f32_x_tied1, svfloat32_t,
		z0 = svcmla_f32_x (p0, z0, z1, z2, 0),
		z0 = svcmla_x (p0, z0, z1, z2, 0))

/*
** cmla_0_f32_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, \1\.s, z2\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f32_x_tied2, svfloat32_t,
		z0 = svcmla_f32_x (p0, z1, z0, z2, 0),
		z0 = svcmla_x (p0, z1, z0, z2, 0))

/*
** cmla_0_f32_x_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, \1\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f32_x_tied3, svfloat32_t,
		z0 = svcmla_f32_x (p0, z1, z2, z0, 0),
		z0 = svcmla_x (p0, z1, z2, z0, 0))

/*
** cmla_0_f32_x_untied:
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, z3\.s, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f32_x_untied, svfloat32_t,
		z0 = svcmla_f32_x (p0, z1, z2, z3, 0),
		z0 = svcmla_x (p0, z1, z2, z3, 0))

/*
** cmla_90_f32_x_tied1:
**	fcmla	z0\.s, p0/m, z1\.s, z2\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f32_x_tied1, svfloat32_t,
		z0 = svcmla_f32_x (p0, z0, z1, z2, 90),
		z0 = svcmla_x (p0, z0, z1, z2, 90))

/*
** cmla_90_f32_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, \1\.s, z2\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f32_x_tied2, svfloat32_t,
		z0 = svcmla_f32_x (p0, z1, z0, z2, 90),
		z0 = svcmla_x (p0, z1, z0, z2, 90))

/*
** cmla_90_f32_x_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, \1\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f32_x_tied3, svfloat32_t,
		z0 = svcmla_f32_x (p0, z1, z2, z0, 90),
		z0 = svcmla_x (p0, z1, z2, z0, 90))

/*
** cmla_90_f32_x_untied:
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, z3\.s, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f32_x_untied, svfloat32_t,
		z0 = svcmla_f32_x (p0, z1, z2, z3, 90),
		z0 = svcmla_x (p0, z1, z2, z3, 90))

/*
** cmla_180_f32_x_tied1:
**	fcmla	z0\.s, p0/m, z1\.s, z2\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f32_x_tied1, svfloat32_t,
		z0 = svcmla_f32_x (p0, z0, z1, z2, 180),
		z0 = svcmla_x (p0, z0, z1, z2, 180))

/*
** cmla_180_f32_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, \1\.s, z2\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f32_x_tied2, svfloat32_t,
		z0 = svcmla_f32_x (p0, z1, z0, z2, 180),
		z0 = svcmla_x (p0, z1, z0, z2, 180))

/*
** cmla_180_f32_x_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, \1\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f32_x_tied3, svfloat32_t,
		z0 = svcmla_f32_x (p0, z1, z2, z0, 180),
		z0 = svcmla_x (p0, z1, z2, z0, 180))

/*
** cmla_180_f32_x_untied:
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, z3\.s, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f32_x_untied, svfloat32_t,
		z0 = svcmla_f32_x (p0, z1, z2, z3, 180),
		z0 = svcmla_x (p0, z1, z2, z3, 180))

/*
** cmla_270_f32_x_tied1:
**	fcmla	z0\.s, p0/m, z1\.s, z2\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f32_x_tied1, svfloat32_t,
		z0 = svcmla_f32_x (p0, z0, z1, z2, 270),
		z0 = svcmla_x (p0, z0, z1, z2, 270))

/*
** cmla_270_f32_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, \1\.s, z2\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f32_x_tied2, svfloat32_t,
		z0 = svcmla_f32_x (p0, z1, z0, z2, 270),
		z0 = svcmla_x (p0, z1, z0, z2, 270))

/*
** cmla_270_f32_x_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, \1\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f32_x_tied3, svfloat32_t,
		z0 = svcmla_f32_x (p0, z1, z2, z0, 270),
		z0 = svcmla_x (p0, z1, z2, z0, 270))

/*
** cmla_270_f32_x_untied:
**	movprfx	z0, z1
**	fcmla	z0\.s, p0/m, z2\.s, z3\.s, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f32_x_untied, svfloat32_t,
		z0 = svcmla_f32_x (p0, z1, z2, z3, 270),
		z0 = svcmla_x (p0, z1, z2, z3, 270))

/*
** ptrue_cmla_0_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_0_f32_x_tied1, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z0, z1, z2, 0),
		z0 = svcmla_x (svptrue_b32 (), z0, z1, z2, 0))

/*
** ptrue_cmla_0_f32_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_0_f32_x_tied2, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z1, z0, z2, 0),
		z0 = svcmla_x (svptrue_b32 (), z1, z0, z2, 0))

/*
** ptrue_cmla_0_f32_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_0_f32_x_tied3, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z1, z2, z0, 0),
		z0 = svcmla_x (svptrue_b32 (), z1, z2, z0, 0))

/*
** ptrue_cmla_0_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_0_f32_x_untied, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z1, z2, z3, 0),
		z0 = svcmla_x (svptrue_b32 (), z1, z2, z3, 0))

/*
** ptrue_cmla_90_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_90_f32_x_tied1, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z0, z1, z2, 90),
		z0 = svcmla_x (svptrue_b32 (), z0, z1, z2, 90))

/*
** ptrue_cmla_90_f32_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_90_f32_x_tied2, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z1, z0, z2, 90),
		z0 = svcmla_x (svptrue_b32 (), z1, z0, z2, 90))

/*
** ptrue_cmla_90_f32_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_90_f32_x_tied3, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z1, z2, z0, 90),
		z0 = svcmla_x (svptrue_b32 (), z1, z2, z0, 90))

/*
** ptrue_cmla_90_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_90_f32_x_untied, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z1, z2, z3, 90),
		z0 = svcmla_x (svptrue_b32 (), z1, z2, z3, 90))

/*
** ptrue_cmla_180_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_180_f32_x_tied1, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z0, z1, z2, 180),
		z0 = svcmla_x (svptrue_b32 (), z0, z1, z2, 180))

/*
** ptrue_cmla_180_f32_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_180_f32_x_tied2, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z1, z0, z2, 180),
		z0 = svcmla_x (svptrue_b32 (), z1, z0, z2, 180))

/*
** ptrue_cmla_180_f32_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_180_f32_x_tied3, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z1, z2, z0, 180),
		z0 = svcmla_x (svptrue_b32 (), z1, z2, z0, 180))

/*
** ptrue_cmla_180_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_180_f32_x_untied, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z1, z2, z3, 180),
		z0 = svcmla_x (svptrue_b32 (), z1, z2, z3, 180))

/*
** ptrue_cmla_270_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_270_f32_x_tied1, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z0, z1, z2, 270),
		z0 = svcmla_x (svptrue_b32 (), z0, z1, z2, 270))

/*
** ptrue_cmla_270_f32_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_270_f32_x_tied2, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z1, z0, z2, 270),
		z0 = svcmla_x (svptrue_b32 (), z1, z0, z2, 270))

/*
** ptrue_cmla_270_f32_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_270_f32_x_tied3, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z1, z2, z0, 270),
		z0 = svcmla_x (svptrue_b32 (), z1, z2, z0, 270))

/*
** ptrue_cmla_270_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_270_f32_x_untied, svfloat32_t,
		z0 = svcmla_f32_x (svptrue_b32 (), z1, z2, z3, 270),
		z0 = svcmla_x (svptrue_b32 (), z1, z2, z3, 270))
