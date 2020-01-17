/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addp_f32_m_tied1:
**	faddp	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (addp_f32_m_tied1, svfloat32_t,
		z0 = svaddp_f32_m (p0, z0, z1),
		z0 = svaddp_m (p0, z0, z1))

/*
** addp_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	faddp	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (addp_f32_m_tied2, svfloat32_t,
		z0 = svaddp_f32_m (p0, z1, z0),
		z0 = svaddp_m (p0, z1, z0))

/*
** addp_f32_m_untied:
**	movprfx	z0, z1
**	faddp	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (addp_f32_m_untied, svfloat32_t,
		z0 = svaddp_f32_m (p0, z1, z2),
		z0 = svaddp_m (p0, z1, z2))

/*
** addp_f32_x_tied1:
**	faddp	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (addp_f32_x_tied1, svfloat32_t,
		z0 = svaddp_f32_x (p0, z0, z1),
		z0 = svaddp_x (p0, z0, z1))

/*
** addp_f32_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	faddp	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (addp_f32_x_tied2, svfloat32_t,
		z0 = svaddp_f32_x (p0, z1, z0),
		z0 = svaddp_x (p0, z1, z0))

/*
** addp_f32_x_untied:
**	movprfx	z0, z1
**	faddp	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (addp_f32_x_untied, svfloat32_t,
		z0 = svaddp_f32_x (p0, z1, z2),
		z0 = svaddp_x (p0, z1, z2))

/*
** ptrue_addp_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_addp_f32_x_tied1, svfloat32_t,
		z0 = svaddp_f32_x (svptrue_b32 (), z0, z1),
		z0 = svaddp_x (svptrue_b32 (), z0, z1))

/*
** ptrue_addp_f32_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_addp_f32_x_tied2, svfloat32_t,
		z0 = svaddp_f32_x (svptrue_b32 (), z1, z0),
		z0 = svaddp_x (svptrue_b32 (), z1, z0))

/*
** ptrue_addp_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_addp_f32_x_untied, svfloat32_t,
		z0 = svaddp_f32_x (svptrue_b32 (), z1, z2),
		z0 = svaddp_x (svptrue_b32 (), z1, z2))
