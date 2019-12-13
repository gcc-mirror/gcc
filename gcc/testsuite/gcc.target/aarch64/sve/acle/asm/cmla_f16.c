/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmla_0_f16_m_tied1:
**	fcmla	z0\.h, p0/m, z1\.h, z2\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f16_m_tied1, svfloat16_t,
		z0 = svcmla_f16_m (p0, z0, z1, z2, 0),
		z0 = svcmla_m (p0, z0, z1, z2, 0))

/*
** cmla_0_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, \1\.h, z2\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f16_m_tied2, svfloat16_t,
		z0 = svcmla_f16_m (p0, z1, z0, z2, 0),
		z0 = svcmla_m (p0, z1, z0, z2, 0))

/*
** cmla_0_f16_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, \1\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f16_m_tied3, svfloat16_t,
		z0 = svcmla_f16_m (p0, z1, z2, z0, 0),
		z0 = svcmla_m (p0, z1, z2, z0, 0))

/*
** cmla_0_f16_m_untied:
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, z3\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f16_m_untied, svfloat16_t,
		z0 = svcmla_f16_m (p0, z1, z2, z3, 0),
		z0 = svcmla_m (p0, z1, z2, z3, 0))

/*
** cmla_90_f16_m_tied1:
**	fcmla	z0\.h, p0/m, z1\.h, z2\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f16_m_tied1, svfloat16_t,
		z0 = svcmla_f16_m (p0, z0, z1, z2, 90),
		z0 = svcmla_m (p0, z0, z1, z2, 90))

/*
** cmla_90_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, \1\.h, z2\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f16_m_tied2, svfloat16_t,
		z0 = svcmla_f16_m (p0, z1, z0, z2, 90),
		z0 = svcmla_m (p0, z1, z0, z2, 90))

/*
** cmla_90_f16_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, \1\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f16_m_tied3, svfloat16_t,
		z0 = svcmla_f16_m (p0, z1, z2, z0, 90),
		z0 = svcmla_m (p0, z1, z2, z0, 90))

/*
** cmla_90_f16_m_untied:
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, z3\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f16_m_untied, svfloat16_t,
		z0 = svcmla_f16_m (p0, z1, z2, z3, 90),
		z0 = svcmla_m (p0, z1, z2, z3, 90))

/*
** cmla_180_f16_m_tied1:
**	fcmla	z0\.h, p0/m, z1\.h, z2\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f16_m_tied1, svfloat16_t,
		z0 = svcmla_f16_m (p0, z0, z1, z2, 180),
		z0 = svcmla_m (p0, z0, z1, z2, 180))

/*
** cmla_180_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, \1\.h, z2\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f16_m_tied2, svfloat16_t,
		z0 = svcmla_f16_m (p0, z1, z0, z2, 180),
		z0 = svcmla_m (p0, z1, z0, z2, 180))

/*
** cmla_180_f16_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, \1\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f16_m_tied3, svfloat16_t,
		z0 = svcmla_f16_m (p0, z1, z2, z0, 180),
		z0 = svcmla_m (p0, z1, z2, z0, 180))

/*
** cmla_180_f16_m_untied:
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, z3\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f16_m_untied, svfloat16_t,
		z0 = svcmla_f16_m (p0, z1, z2, z3, 180),
		z0 = svcmla_m (p0, z1, z2, z3, 180))

/*
** cmla_270_f16_m_tied1:
**	fcmla	z0\.h, p0/m, z1\.h, z2\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f16_m_tied1, svfloat16_t,
		z0 = svcmla_f16_m (p0, z0, z1, z2, 270),
		z0 = svcmla_m (p0, z0, z1, z2, 270))

/*
** cmla_270_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, \1\.h, z2\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f16_m_tied2, svfloat16_t,
		z0 = svcmla_f16_m (p0, z1, z0, z2, 270),
		z0 = svcmla_m (p0, z1, z0, z2, 270))

/*
** cmla_270_f16_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, \1\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f16_m_tied3, svfloat16_t,
		z0 = svcmla_f16_m (p0, z1, z2, z0, 270),
		z0 = svcmla_m (p0, z1, z2, z0, 270))

/*
** cmla_270_f16_m_untied:
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, z3\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f16_m_untied, svfloat16_t,
		z0 = svcmla_f16_m (p0, z1, z2, z3, 270),
		z0 = svcmla_m (p0, z1, z2, z3, 270))

/*
** cmla_0_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fcmla	z0\.h, p0/m, z1\.h, z2\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f16_z_tied1, svfloat16_t,
		z0 = svcmla_f16_z (p0, z0, z1, z2, 0),
		z0 = svcmla_z (p0, z0, z1, z2, 0))

/*
** cmla_0_f16_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z1\.h
**	fcmla	z0\.h, p0/m, \1\.h, z2\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f16_z_tied2, svfloat16_t,
		z0 = svcmla_f16_z (p0, z1, z0, z2, 0),
		z0 = svcmla_z (p0, z1, z0, z2, 0))

/*
** cmla_0_f16_z_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z1\.h
**	fcmla	z0\.h, p0/m, z2\.h, \1\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f16_z_tied3, svfloat16_t,
		z0 = svcmla_f16_z (p0, z1, z2, z0, 0),
		z0 = svcmla_z (p0, z1, z2, z0, 0))

/*
** cmla_0_f16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	fcmla	z0\.h, p0/m, z2\.h, z3\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f16_z_untied, svfloat16_t,
		z0 = svcmla_f16_z (p0, z1, z2, z3, 0),
		z0 = svcmla_z (p0, z1, z2, z3, 0))

/*
** cmla_90_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fcmla	z0\.h, p0/m, z1\.h, z2\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f16_z_tied1, svfloat16_t,
		z0 = svcmla_f16_z (p0, z0, z1, z2, 90),
		z0 = svcmla_z (p0, z0, z1, z2, 90))

/*
** cmla_90_f16_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z1\.h
**	fcmla	z0\.h, p0/m, \1\.h, z2\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f16_z_tied2, svfloat16_t,
		z0 = svcmla_f16_z (p0, z1, z0, z2, 90),
		z0 = svcmla_z (p0, z1, z0, z2, 90))

/*
** cmla_90_f16_z_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z1\.h
**	fcmla	z0\.h, p0/m, z2\.h, \1\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f16_z_tied3, svfloat16_t,
		z0 = svcmla_f16_z (p0, z1, z2, z0, 90),
		z0 = svcmla_z (p0, z1, z2, z0, 90))

/*
** cmla_90_f16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	fcmla	z0\.h, p0/m, z2\.h, z3\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f16_z_untied, svfloat16_t,
		z0 = svcmla_f16_z (p0, z1, z2, z3, 90),
		z0 = svcmla_z (p0, z1, z2, z3, 90))

/*
** cmla_180_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fcmla	z0\.h, p0/m, z1\.h, z2\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f16_z_tied1, svfloat16_t,
		z0 = svcmla_f16_z (p0, z0, z1, z2, 180),
		z0 = svcmla_z (p0, z0, z1, z2, 180))

/*
** cmla_180_f16_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z1\.h
**	fcmla	z0\.h, p0/m, \1\.h, z2\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f16_z_tied2, svfloat16_t,
		z0 = svcmla_f16_z (p0, z1, z0, z2, 180),
		z0 = svcmla_z (p0, z1, z0, z2, 180))

/*
** cmla_180_f16_z_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z1\.h
**	fcmla	z0\.h, p0/m, z2\.h, \1\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f16_z_tied3, svfloat16_t,
		z0 = svcmla_f16_z (p0, z1, z2, z0, 180),
		z0 = svcmla_z (p0, z1, z2, z0, 180))

/*
** cmla_180_f16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	fcmla	z0\.h, p0/m, z2\.h, z3\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f16_z_untied, svfloat16_t,
		z0 = svcmla_f16_z (p0, z1, z2, z3, 180),
		z0 = svcmla_z (p0, z1, z2, z3, 180))

/*
** cmla_270_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fcmla	z0\.h, p0/m, z1\.h, z2\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f16_z_tied1, svfloat16_t,
		z0 = svcmla_f16_z (p0, z0, z1, z2, 270),
		z0 = svcmla_z (p0, z0, z1, z2, 270))

/*
** cmla_270_f16_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z1\.h
**	fcmla	z0\.h, p0/m, \1\.h, z2\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f16_z_tied2, svfloat16_t,
		z0 = svcmla_f16_z (p0, z1, z0, z2, 270),
		z0 = svcmla_z (p0, z1, z0, z2, 270))

/*
** cmla_270_f16_z_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z1\.h
**	fcmla	z0\.h, p0/m, z2\.h, \1\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f16_z_tied3, svfloat16_t,
		z0 = svcmla_f16_z (p0, z1, z2, z0, 270),
		z0 = svcmla_z (p0, z1, z2, z0, 270))

/*
** cmla_270_f16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	fcmla	z0\.h, p0/m, z2\.h, z3\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f16_z_untied, svfloat16_t,
		z0 = svcmla_f16_z (p0, z1, z2, z3, 270),
		z0 = svcmla_z (p0, z1, z2, z3, 270))

/*
** cmla_0_f16_x_tied1:
**	fcmla	z0\.h, p0/m, z1\.h, z2\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f16_x_tied1, svfloat16_t,
		z0 = svcmla_f16_x (p0, z0, z1, z2, 0),
		z0 = svcmla_x (p0, z0, z1, z2, 0))

/*
** cmla_0_f16_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, \1\.h, z2\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f16_x_tied2, svfloat16_t,
		z0 = svcmla_f16_x (p0, z1, z0, z2, 0),
		z0 = svcmla_x (p0, z1, z0, z2, 0))

/*
** cmla_0_f16_x_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, \1\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f16_x_tied3, svfloat16_t,
		z0 = svcmla_f16_x (p0, z1, z2, z0, 0),
		z0 = svcmla_x (p0, z1, z2, z0, 0))

/*
** cmla_0_f16_x_untied:
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, z3\.h, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f16_x_untied, svfloat16_t,
		z0 = svcmla_f16_x (p0, z1, z2, z3, 0),
		z0 = svcmla_x (p0, z1, z2, z3, 0))

/*
** cmla_90_f16_x_tied1:
**	fcmla	z0\.h, p0/m, z1\.h, z2\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f16_x_tied1, svfloat16_t,
		z0 = svcmla_f16_x (p0, z0, z1, z2, 90),
		z0 = svcmla_x (p0, z0, z1, z2, 90))

/*
** cmla_90_f16_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, \1\.h, z2\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f16_x_tied2, svfloat16_t,
		z0 = svcmla_f16_x (p0, z1, z0, z2, 90),
		z0 = svcmla_x (p0, z1, z0, z2, 90))

/*
** cmla_90_f16_x_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, \1\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f16_x_tied3, svfloat16_t,
		z0 = svcmla_f16_x (p0, z1, z2, z0, 90),
		z0 = svcmla_x (p0, z1, z2, z0, 90))

/*
** cmla_90_f16_x_untied:
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, z3\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f16_x_untied, svfloat16_t,
		z0 = svcmla_f16_x (p0, z1, z2, z3, 90),
		z0 = svcmla_x (p0, z1, z2, z3, 90))

/*
** cmla_180_f16_x_tied1:
**	fcmla	z0\.h, p0/m, z1\.h, z2\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f16_x_tied1, svfloat16_t,
		z0 = svcmla_f16_x (p0, z0, z1, z2, 180),
		z0 = svcmla_x (p0, z0, z1, z2, 180))

/*
** cmla_180_f16_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, \1\.h, z2\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f16_x_tied2, svfloat16_t,
		z0 = svcmla_f16_x (p0, z1, z0, z2, 180),
		z0 = svcmla_x (p0, z1, z0, z2, 180))

/*
** cmla_180_f16_x_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, \1\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f16_x_tied3, svfloat16_t,
		z0 = svcmla_f16_x (p0, z1, z2, z0, 180),
		z0 = svcmla_x (p0, z1, z2, z0, 180))

/*
** cmla_180_f16_x_untied:
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, z3\.h, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f16_x_untied, svfloat16_t,
		z0 = svcmla_f16_x (p0, z1, z2, z3, 180),
		z0 = svcmla_x (p0, z1, z2, z3, 180))

/*
** cmla_270_f16_x_tied1:
**	fcmla	z0\.h, p0/m, z1\.h, z2\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f16_x_tied1, svfloat16_t,
		z0 = svcmla_f16_x (p0, z0, z1, z2, 270),
		z0 = svcmla_x (p0, z0, z1, z2, 270))

/*
** cmla_270_f16_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, \1\.h, z2\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f16_x_tied2, svfloat16_t,
		z0 = svcmla_f16_x (p0, z1, z0, z2, 270),
		z0 = svcmla_x (p0, z1, z0, z2, 270))

/*
** cmla_270_f16_x_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, \1\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f16_x_tied3, svfloat16_t,
		z0 = svcmla_f16_x (p0, z1, z2, z0, 270),
		z0 = svcmla_x (p0, z1, z2, z0, 270))

/*
** cmla_270_f16_x_untied:
**	movprfx	z0, z1
**	fcmla	z0\.h, p0/m, z2\.h, z3\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f16_x_untied, svfloat16_t,
		z0 = svcmla_f16_x (p0, z1, z2, z3, 270),
		z0 = svcmla_x (p0, z1, z2, z3, 270))

/*
** ptrue_cmla_0_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_0_f16_x_tied1, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z0, z1, z2, 0),
		z0 = svcmla_x (svptrue_b16 (), z0, z1, z2, 0))

/*
** ptrue_cmla_0_f16_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_0_f16_x_tied2, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z1, z0, z2, 0),
		z0 = svcmla_x (svptrue_b16 (), z1, z0, z2, 0))

/*
** ptrue_cmla_0_f16_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_0_f16_x_tied3, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z1, z2, z0, 0),
		z0 = svcmla_x (svptrue_b16 (), z1, z2, z0, 0))

/*
** ptrue_cmla_0_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_0_f16_x_untied, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z1, z2, z3, 0),
		z0 = svcmla_x (svptrue_b16 (), z1, z2, z3, 0))

/*
** ptrue_cmla_90_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_90_f16_x_tied1, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z0, z1, z2, 90),
		z0 = svcmla_x (svptrue_b16 (), z0, z1, z2, 90))

/*
** ptrue_cmla_90_f16_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_90_f16_x_tied2, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z1, z0, z2, 90),
		z0 = svcmla_x (svptrue_b16 (), z1, z0, z2, 90))

/*
** ptrue_cmla_90_f16_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_90_f16_x_tied3, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z1, z2, z0, 90),
		z0 = svcmla_x (svptrue_b16 (), z1, z2, z0, 90))

/*
** ptrue_cmla_90_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_90_f16_x_untied, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z1, z2, z3, 90),
		z0 = svcmla_x (svptrue_b16 (), z1, z2, z3, 90))

/*
** ptrue_cmla_180_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_180_f16_x_tied1, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z0, z1, z2, 180),
		z0 = svcmla_x (svptrue_b16 (), z0, z1, z2, 180))

/*
** ptrue_cmla_180_f16_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_180_f16_x_tied2, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z1, z0, z2, 180),
		z0 = svcmla_x (svptrue_b16 (), z1, z0, z2, 180))

/*
** ptrue_cmla_180_f16_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_180_f16_x_tied3, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z1, z2, z0, 180),
		z0 = svcmla_x (svptrue_b16 (), z1, z2, z0, 180))

/*
** ptrue_cmla_180_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_180_f16_x_untied, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z1, z2, z3, 180),
		z0 = svcmla_x (svptrue_b16 (), z1, z2, z3, 180))

/*
** ptrue_cmla_270_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_270_f16_x_tied1, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z0, z1, z2, 270),
		z0 = svcmla_x (svptrue_b16 (), z0, z1, z2, 270))

/*
** ptrue_cmla_270_f16_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_270_f16_x_tied2, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z1, z0, z2, 270),
		z0 = svcmla_x (svptrue_b16 (), z1, z0, z2, 270))

/*
** ptrue_cmla_270_f16_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_270_f16_x_tied3, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z1, z2, z0, 270),
		z0 = svcmla_x (svptrue_b16 (), z1, z2, z0, 270))

/*
** ptrue_cmla_270_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_270_f16_x_untied, svfloat16_t,
		z0 = svcmla_f16_x (svptrue_b16 (), z1, z2, z3, 270),
		z0 = svcmla_x (svptrue_b16 (), z1, z2, z3, 270))
