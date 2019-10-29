/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmla_0_f64_m_tied1:
**	fcmla	z0\.d, p0/m, z1\.d, z2\.d, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f64_m_tied1, svfloat64_t,
		z0 = svcmla_f64_m (p0, z0, z1, z2, 0),
		z0 = svcmla_m (p0, z0, z1, z2, 0))

/*
** cmla_0_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, \1, z2\.d, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f64_m_tied2, svfloat64_t,
		z0 = svcmla_f64_m (p0, z1, z0, z2, 0),
		z0 = svcmla_m (p0, z1, z0, z2, 0))

/*
** cmla_0_f64_m_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, \1, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f64_m_tied3, svfloat64_t,
		z0 = svcmla_f64_m (p0, z1, z2, z0, 0),
		z0 = svcmla_m (p0, z1, z2, z0, 0))

/*
** cmla_0_f64_m_untied:
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, z3\.d, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f64_m_untied, svfloat64_t,
		z0 = svcmla_f64_m (p0, z1, z2, z3, 0),
		z0 = svcmla_m (p0, z1, z2, z3, 0))

/*
** cmla_90_f64_m_tied1:
**	fcmla	z0\.d, p0/m, z1\.d, z2\.d, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f64_m_tied1, svfloat64_t,
		z0 = svcmla_f64_m (p0, z0, z1, z2, 90),
		z0 = svcmla_m (p0, z0, z1, z2, 90))

/*
** cmla_90_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, \1, z2\.d, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f64_m_tied2, svfloat64_t,
		z0 = svcmla_f64_m (p0, z1, z0, z2, 90),
		z0 = svcmla_m (p0, z1, z0, z2, 90))

/*
** cmla_90_f64_m_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, \1, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f64_m_tied3, svfloat64_t,
		z0 = svcmla_f64_m (p0, z1, z2, z0, 90),
		z0 = svcmla_m (p0, z1, z2, z0, 90))

/*
** cmla_90_f64_m_untied:
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, z3\.d, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f64_m_untied, svfloat64_t,
		z0 = svcmla_f64_m (p0, z1, z2, z3, 90),
		z0 = svcmla_m (p0, z1, z2, z3, 90))

/*
** cmla_180_f64_m_tied1:
**	fcmla	z0\.d, p0/m, z1\.d, z2\.d, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f64_m_tied1, svfloat64_t,
		z0 = svcmla_f64_m (p0, z0, z1, z2, 180),
		z0 = svcmla_m (p0, z0, z1, z2, 180))

/*
** cmla_180_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, \1, z2\.d, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f64_m_tied2, svfloat64_t,
		z0 = svcmla_f64_m (p0, z1, z0, z2, 180),
		z0 = svcmla_m (p0, z1, z0, z2, 180))

/*
** cmla_180_f64_m_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, \1, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f64_m_tied3, svfloat64_t,
		z0 = svcmla_f64_m (p0, z1, z2, z0, 180),
		z0 = svcmla_m (p0, z1, z2, z0, 180))

/*
** cmla_180_f64_m_untied:
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, z3\.d, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f64_m_untied, svfloat64_t,
		z0 = svcmla_f64_m (p0, z1, z2, z3, 180),
		z0 = svcmla_m (p0, z1, z2, z3, 180))

/*
** cmla_270_f64_m_tied1:
**	fcmla	z0\.d, p0/m, z1\.d, z2\.d, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f64_m_tied1, svfloat64_t,
		z0 = svcmla_f64_m (p0, z0, z1, z2, 270),
		z0 = svcmla_m (p0, z0, z1, z2, 270))

/*
** cmla_270_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, \1, z2\.d, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f64_m_tied2, svfloat64_t,
		z0 = svcmla_f64_m (p0, z1, z0, z2, 270),
		z0 = svcmla_m (p0, z1, z0, z2, 270))

/*
** cmla_270_f64_m_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, \1, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f64_m_tied3, svfloat64_t,
		z0 = svcmla_f64_m (p0, z1, z2, z0, 270),
		z0 = svcmla_m (p0, z1, z2, z0, 270))

/*
** cmla_270_f64_m_untied:
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, z3\.d, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f64_m_untied, svfloat64_t,
		z0 = svcmla_f64_m (p0, z1, z2, z3, 270),
		z0 = svcmla_m (p0, z1, z2, z3, 270))

/*
** cmla_0_f64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	fcmla	z0\.d, p0/m, z1\.d, z2\.d, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f64_z_tied1, svfloat64_t,
		z0 = svcmla_f64_z (p0, z0, z1, z2, 0),
		z0 = svcmla_z (p0, z0, z1, z2, 0))

/*
** cmla_0_f64_z_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, z1\.d
**	fcmla	z0\.d, p0/m, \1, z2\.d, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f64_z_tied2, svfloat64_t,
		z0 = svcmla_f64_z (p0, z1, z0, z2, 0),
		z0 = svcmla_z (p0, z1, z0, z2, 0))

/*
** cmla_0_f64_z_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, z1\.d
**	fcmla	z0\.d, p0/m, z2\.d, \1, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f64_z_tied3, svfloat64_t,
		z0 = svcmla_f64_z (p0, z1, z2, z0, 0),
		z0 = svcmla_z (p0, z1, z2, z0, 0))

/*
** cmla_0_f64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	fcmla	z0\.d, p0/m, z2\.d, z3\.d, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f64_z_untied, svfloat64_t,
		z0 = svcmla_f64_z (p0, z1, z2, z3, 0),
		z0 = svcmla_z (p0, z1, z2, z3, 0))

/*
** cmla_90_f64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	fcmla	z0\.d, p0/m, z1\.d, z2\.d, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f64_z_tied1, svfloat64_t,
		z0 = svcmla_f64_z (p0, z0, z1, z2, 90),
		z0 = svcmla_z (p0, z0, z1, z2, 90))

/*
** cmla_90_f64_z_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, z1\.d
**	fcmla	z0\.d, p0/m, \1, z2\.d, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f64_z_tied2, svfloat64_t,
		z0 = svcmla_f64_z (p0, z1, z0, z2, 90),
		z0 = svcmla_z (p0, z1, z0, z2, 90))

/*
** cmla_90_f64_z_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, z1\.d
**	fcmla	z0\.d, p0/m, z2\.d, \1, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f64_z_tied3, svfloat64_t,
		z0 = svcmla_f64_z (p0, z1, z2, z0, 90),
		z0 = svcmla_z (p0, z1, z2, z0, 90))

/*
** cmla_90_f64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	fcmla	z0\.d, p0/m, z2\.d, z3\.d, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f64_z_untied, svfloat64_t,
		z0 = svcmla_f64_z (p0, z1, z2, z3, 90),
		z0 = svcmla_z (p0, z1, z2, z3, 90))

/*
** cmla_180_f64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	fcmla	z0\.d, p0/m, z1\.d, z2\.d, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f64_z_tied1, svfloat64_t,
		z0 = svcmla_f64_z (p0, z0, z1, z2, 180),
		z0 = svcmla_z (p0, z0, z1, z2, 180))

/*
** cmla_180_f64_z_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, z1\.d
**	fcmla	z0\.d, p0/m, \1, z2\.d, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f64_z_tied2, svfloat64_t,
		z0 = svcmla_f64_z (p0, z1, z0, z2, 180),
		z0 = svcmla_z (p0, z1, z0, z2, 180))

/*
** cmla_180_f64_z_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, z1\.d
**	fcmla	z0\.d, p0/m, z2\.d, \1, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f64_z_tied3, svfloat64_t,
		z0 = svcmla_f64_z (p0, z1, z2, z0, 180),
		z0 = svcmla_z (p0, z1, z2, z0, 180))

/*
** cmla_180_f64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	fcmla	z0\.d, p0/m, z2\.d, z3\.d, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f64_z_untied, svfloat64_t,
		z0 = svcmla_f64_z (p0, z1, z2, z3, 180),
		z0 = svcmla_z (p0, z1, z2, z3, 180))

/*
** cmla_270_f64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	fcmla	z0\.d, p0/m, z1\.d, z2\.d, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f64_z_tied1, svfloat64_t,
		z0 = svcmla_f64_z (p0, z0, z1, z2, 270),
		z0 = svcmla_z (p0, z0, z1, z2, 270))

/*
** cmla_270_f64_z_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, z1\.d
**	fcmla	z0\.d, p0/m, \1, z2\.d, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f64_z_tied2, svfloat64_t,
		z0 = svcmla_f64_z (p0, z1, z0, z2, 270),
		z0 = svcmla_z (p0, z1, z0, z2, 270))

/*
** cmla_270_f64_z_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, z1\.d
**	fcmla	z0\.d, p0/m, z2\.d, \1, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f64_z_tied3, svfloat64_t,
		z0 = svcmla_f64_z (p0, z1, z2, z0, 270),
		z0 = svcmla_z (p0, z1, z2, z0, 270))

/*
** cmla_270_f64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	fcmla	z0\.d, p0/m, z2\.d, z3\.d, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f64_z_untied, svfloat64_t,
		z0 = svcmla_f64_z (p0, z1, z2, z3, 270),
		z0 = svcmla_z (p0, z1, z2, z3, 270))

/*
** cmla_0_f64_x_tied1:
**	fcmla	z0\.d, p0/m, z1\.d, z2\.d, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f64_x_tied1, svfloat64_t,
		z0 = svcmla_f64_x (p0, z0, z1, z2, 0),
		z0 = svcmla_x (p0, z0, z1, z2, 0))

/*
** cmla_0_f64_x_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, \1, z2\.d, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f64_x_tied2, svfloat64_t,
		z0 = svcmla_f64_x (p0, z1, z0, z2, 0),
		z0 = svcmla_x (p0, z1, z0, z2, 0))

/*
** cmla_0_f64_x_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, \1, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f64_x_tied3, svfloat64_t,
		z0 = svcmla_f64_x (p0, z1, z2, z0, 0),
		z0 = svcmla_x (p0, z1, z2, z0, 0))

/*
** cmla_0_f64_x_untied:
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, z3\.d, #0
**	ret
*/
TEST_UNIFORM_Z (cmla_0_f64_x_untied, svfloat64_t,
		z0 = svcmla_f64_x (p0, z1, z2, z3, 0),
		z0 = svcmla_x (p0, z1, z2, z3, 0))

/*
** cmla_90_f64_x_tied1:
**	fcmla	z0\.d, p0/m, z1\.d, z2\.d, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f64_x_tied1, svfloat64_t,
		z0 = svcmla_f64_x (p0, z0, z1, z2, 90),
		z0 = svcmla_x (p0, z0, z1, z2, 90))

/*
** cmla_90_f64_x_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, \1, z2\.d, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f64_x_tied2, svfloat64_t,
		z0 = svcmla_f64_x (p0, z1, z0, z2, 90),
		z0 = svcmla_x (p0, z1, z0, z2, 90))

/*
** cmla_90_f64_x_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, \1, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f64_x_tied3, svfloat64_t,
		z0 = svcmla_f64_x (p0, z1, z2, z0, 90),
		z0 = svcmla_x (p0, z1, z2, z0, 90))

/*
** cmla_90_f64_x_untied:
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, z3\.d, #90
**	ret
*/
TEST_UNIFORM_Z (cmla_90_f64_x_untied, svfloat64_t,
		z0 = svcmla_f64_x (p0, z1, z2, z3, 90),
		z0 = svcmla_x (p0, z1, z2, z3, 90))

/*
** cmla_180_f64_x_tied1:
**	fcmla	z0\.d, p0/m, z1\.d, z2\.d, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f64_x_tied1, svfloat64_t,
		z0 = svcmla_f64_x (p0, z0, z1, z2, 180),
		z0 = svcmla_x (p0, z0, z1, z2, 180))

/*
** cmla_180_f64_x_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, \1, z2\.d, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f64_x_tied2, svfloat64_t,
		z0 = svcmla_f64_x (p0, z1, z0, z2, 180),
		z0 = svcmla_x (p0, z1, z0, z2, 180))

/*
** cmla_180_f64_x_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, \1, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f64_x_tied3, svfloat64_t,
		z0 = svcmla_f64_x (p0, z1, z2, z0, 180),
		z0 = svcmla_x (p0, z1, z2, z0, 180))

/*
** cmla_180_f64_x_untied:
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, z3\.d, #180
**	ret
*/
TEST_UNIFORM_Z (cmla_180_f64_x_untied, svfloat64_t,
		z0 = svcmla_f64_x (p0, z1, z2, z3, 180),
		z0 = svcmla_x (p0, z1, z2, z3, 180))

/*
** cmla_270_f64_x_tied1:
**	fcmla	z0\.d, p0/m, z1\.d, z2\.d, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f64_x_tied1, svfloat64_t,
		z0 = svcmla_f64_x (p0, z0, z1, z2, 270),
		z0 = svcmla_x (p0, z0, z1, z2, 270))

/*
** cmla_270_f64_x_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, \1, z2\.d, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f64_x_tied2, svfloat64_t,
		z0 = svcmla_f64_x (p0, z1, z0, z2, 270),
		z0 = svcmla_x (p0, z1, z0, z2, 270))

/*
** cmla_270_f64_x_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, \1, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f64_x_tied3, svfloat64_t,
		z0 = svcmla_f64_x (p0, z1, z2, z0, 270),
		z0 = svcmla_x (p0, z1, z2, z0, 270))

/*
** cmla_270_f64_x_untied:
**	movprfx	z0, z1
**	fcmla	z0\.d, p0/m, z2\.d, z3\.d, #270
**	ret
*/
TEST_UNIFORM_Z (cmla_270_f64_x_untied, svfloat64_t,
		z0 = svcmla_f64_x (p0, z1, z2, z3, 270),
		z0 = svcmla_x (p0, z1, z2, z3, 270))

/*
** ptrue_cmla_0_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_0_f64_x_tied1, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z0, z1, z2, 0),
		z0 = svcmla_x (svptrue_b64 (), z0, z1, z2, 0))

/*
** ptrue_cmla_0_f64_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_0_f64_x_tied2, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z1, z0, z2, 0),
		z0 = svcmla_x (svptrue_b64 (), z1, z0, z2, 0))

/*
** ptrue_cmla_0_f64_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_0_f64_x_tied3, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z1, z2, z0, 0),
		z0 = svcmla_x (svptrue_b64 (), z1, z2, z0, 0))

/*
** ptrue_cmla_0_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_0_f64_x_untied, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z1, z2, z3, 0),
		z0 = svcmla_x (svptrue_b64 (), z1, z2, z3, 0))

/*
** ptrue_cmla_90_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_90_f64_x_tied1, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z0, z1, z2, 90),
		z0 = svcmla_x (svptrue_b64 (), z0, z1, z2, 90))

/*
** ptrue_cmla_90_f64_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_90_f64_x_tied2, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z1, z0, z2, 90),
		z0 = svcmla_x (svptrue_b64 (), z1, z0, z2, 90))

/*
** ptrue_cmla_90_f64_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_90_f64_x_tied3, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z1, z2, z0, 90),
		z0 = svcmla_x (svptrue_b64 (), z1, z2, z0, 90))

/*
** ptrue_cmla_90_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_90_f64_x_untied, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z1, z2, z3, 90),
		z0 = svcmla_x (svptrue_b64 (), z1, z2, z3, 90))

/*
** ptrue_cmla_180_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_180_f64_x_tied1, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z0, z1, z2, 180),
		z0 = svcmla_x (svptrue_b64 (), z0, z1, z2, 180))

/*
** ptrue_cmla_180_f64_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_180_f64_x_tied2, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z1, z0, z2, 180),
		z0 = svcmla_x (svptrue_b64 (), z1, z0, z2, 180))

/*
** ptrue_cmla_180_f64_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_180_f64_x_tied3, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z1, z2, z0, 180),
		z0 = svcmla_x (svptrue_b64 (), z1, z2, z0, 180))

/*
** ptrue_cmla_180_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_180_f64_x_untied, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z1, z2, z3, 180),
		z0 = svcmla_x (svptrue_b64 (), z1, z2, z3, 180))

/*
** ptrue_cmla_270_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_270_f64_x_tied1, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z0, z1, z2, 270),
		z0 = svcmla_x (svptrue_b64 (), z0, z1, z2, 270))

/*
** ptrue_cmla_270_f64_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_270_f64_x_tied2, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z1, z0, z2, 270),
		z0 = svcmla_x (svptrue_b64 (), z1, z0, z2, 270))

/*
** ptrue_cmla_270_f64_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_270_f64_x_tied3, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z1, z2, z0, 270),
		z0 = svcmla_x (svptrue_b64 (), z1, z2, z0, 270))

/*
** ptrue_cmla_270_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cmla_270_f64_x_untied, svfloat64_t,
		z0 = svcmla_f64_x (svptrue_b64 (), z1, z2, z3, 270),
		z0 = svcmla_x (svptrue_b64 (), z1, z2, z3, 270))
