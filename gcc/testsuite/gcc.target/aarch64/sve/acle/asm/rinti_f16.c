/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rinti_f16_m_tied12:
**	frinti	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_UNIFORM_Z (rinti_f16_m_tied12, svfloat16_t,
		z0 = svrinti_f16_m (z0, p0, z0),
		z0 = svrinti_m (z0, p0, z0))

/*
** rinti_f16_m_tied1:
**	frinti	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rinti_f16_m_tied1, svfloat16_t,
		z0 = svrinti_f16_m (z0, p0, z1),
		z0 = svrinti_m (z0, p0, z1))

/*
** rinti_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	frinti	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_UNIFORM_Z (rinti_f16_m_tied2, svfloat16_t,
		z0 = svrinti_f16_m (z1, p0, z0),
		z0 = svrinti_m (z1, p0, z0))

/*
** rinti_f16_m_untied:
**	movprfx	z0, z2
**	frinti	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rinti_f16_m_untied, svfloat16_t,
		z0 = svrinti_f16_m (z2, p0, z1),
		z0 = svrinti_m (z2, p0, z1))

/*
** rinti_f16_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, \1\.h
**	frinti	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_UNIFORM_Z (rinti_f16_z_tied1, svfloat16_t,
		z0 = svrinti_f16_z (p0, z0),
		z0 = svrinti_z (p0, z0))

/*
** rinti_f16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	frinti	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rinti_f16_z_untied, svfloat16_t,
		z0 = svrinti_f16_z (p0, z1),
		z0 = svrinti_z (p0, z1))

/*
** rinti_f16_x_tied1:
**	frinti	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_UNIFORM_Z (rinti_f16_x_tied1, svfloat16_t,
		z0 = svrinti_f16_x (p0, z0),
		z0 = svrinti_x (p0, z0))

/*
** rinti_f16_x_untied:
**	movprfx	z0, z1
**	frinti	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rinti_f16_x_untied, svfloat16_t,
		z0 = svrinti_f16_x (p0, z1),
		z0 = svrinti_x (p0, z1))

/*
** ptrue_rinti_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_rinti_f16_x_tied1, svfloat16_t,
		z0 = svrinti_f16_x (svptrue_b16 (), z0),
		z0 = svrinti_x (svptrue_b16 (), z0))

/*
** ptrue_rinti_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_rinti_f16_x_untied, svfloat16_t,
		z0 = svrinti_f16_x (svptrue_b16 (), z1),
		z0 = svrinti_x (svptrue_b16 (), z1))
