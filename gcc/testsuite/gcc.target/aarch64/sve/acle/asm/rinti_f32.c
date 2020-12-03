/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rinti_f32_m_tied12:
**	frinti	z0\.s, p0/m, z0\.s
**	ret
*/
TEST_UNIFORM_Z (rinti_f32_m_tied12, svfloat32_t,
		z0 = svrinti_f32_m (z0, p0, z0),
		z0 = svrinti_m (z0, p0, z0))

/*
** rinti_f32_m_tied1:
**	frinti	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rinti_f32_m_tied1, svfloat32_t,
		z0 = svrinti_f32_m (z0, p0, z1),
		z0 = svrinti_m (z0, p0, z1))

/*
** rinti_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	frinti	z0\.s, p0/m, \1\.s
**	ret
*/
TEST_UNIFORM_Z (rinti_f32_m_tied2, svfloat32_t,
		z0 = svrinti_f32_m (z1, p0, z0),
		z0 = svrinti_m (z1, p0, z0))

/*
** rinti_f32_m_untied:
**	movprfx	z0, z2
**	frinti	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rinti_f32_m_untied, svfloat32_t,
		z0 = svrinti_f32_m (z2, p0, z1),
		z0 = svrinti_m (z2, p0, z1))

/*
** rinti_f32_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, \1\.s
**	frinti	z0\.s, p0/m, \1\.s
**	ret
*/
TEST_UNIFORM_Z (rinti_f32_z_tied1, svfloat32_t,
		z0 = svrinti_f32_z (p0, z0),
		z0 = svrinti_z (p0, z0))

/*
** rinti_f32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	frinti	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rinti_f32_z_untied, svfloat32_t,
		z0 = svrinti_f32_z (p0, z1),
		z0 = svrinti_z (p0, z1))

/*
** rinti_f32_x_tied1:
**	frinti	z0\.s, p0/m, z0\.s
**	ret
*/
TEST_UNIFORM_Z (rinti_f32_x_tied1, svfloat32_t,
		z0 = svrinti_f32_x (p0, z0),
		z0 = svrinti_x (p0, z0))

/*
** rinti_f32_x_untied:
**	movprfx	z0, z1
**	frinti	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (rinti_f32_x_untied, svfloat32_t,
		z0 = svrinti_f32_x (p0, z1),
		z0 = svrinti_x (p0, z1))

/*
** ptrue_rinti_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_rinti_f32_x_tied1, svfloat32_t,
		z0 = svrinti_f32_x (svptrue_b32 (), z0),
		z0 = svrinti_x (svptrue_b32 (), z0))

/*
** ptrue_rinti_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_rinti_f32_x_untied, svfloat32_t,
		z0 = svrinti_f32_x (svptrue_b32 (), z1),
		z0 = svrinti_x (svptrue_b32 (), z1))
