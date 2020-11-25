/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rintn_f32_m_tied12:
**	frintn	z0\.s, p0/m, z0\.s
**	ret
*/
TEST_UNIFORM_Z (rintn_f32_m_tied12, svfloat32_t,
		z0 = svrintn_f32_m (z0, p0, z0),
		z0 = svrintn_m (z0, p0, z0))

/*
** rintn_f32_m_tied1:
**	frintn	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rintn_f32_m_tied1, svfloat32_t,
		z0 = svrintn_f32_m (z0, p0, z1),
		z0 = svrintn_m (z0, p0, z1))

/*
** rintn_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	frintn	z0\.s, p0/m, \1\.s
**	ret
*/
TEST_UNIFORM_Z (rintn_f32_m_tied2, svfloat32_t,
		z0 = svrintn_f32_m (z1, p0, z0),
		z0 = svrintn_m (z1, p0, z0))

/*
** rintn_f32_m_untied:
**	movprfx	z0, z2
**	frintn	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rintn_f32_m_untied, svfloat32_t,
		z0 = svrintn_f32_m (z2, p0, z1),
		z0 = svrintn_m (z2, p0, z1))

/*
** rintn_f32_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, \1\.s
**	frintn	z0\.s, p0/m, \1\.s
**	ret
*/
TEST_UNIFORM_Z (rintn_f32_z_tied1, svfloat32_t,
		z0 = svrintn_f32_z (p0, z0),
		z0 = svrintn_z (p0, z0))

/*
** rintn_f32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	frintn	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rintn_f32_z_untied, svfloat32_t,
		z0 = svrintn_f32_z (p0, z1),
		z0 = svrintn_z (p0, z1))

/*
** rintn_f32_x_tied1:
**	frintn	z0\.s, p0/m, z0\.s
**	ret
*/
TEST_UNIFORM_Z (rintn_f32_x_tied1, svfloat32_t,
		z0 = svrintn_f32_x (p0, z0),
		z0 = svrintn_x (p0, z0))

/*
** rintn_f32_x_untied:
**	movprfx	z0, z1
**	frintn	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rintn_f32_x_untied, svfloat32_t,
		z0 = svrintn_f32_x (p0, z1),
		z0 = svrintn_x (p0, z1))

/*
** ptrue_rintn_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_rintn_f32_x_tied1, svfloat32_t,
		z0 = svrintn_f32_x (svptrue_b32 (), z0),
		z0 = svrintn_x (svptrue_b32 (), z0))

/*
** ptrue_rintn_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_rintn_f32_x_untied, svfloat32_t,
		z0 = svrintn_f32_x (svptrue_b32 (), z1),
		z0 = svrintn_x (svptrue_b32 (), z1))
