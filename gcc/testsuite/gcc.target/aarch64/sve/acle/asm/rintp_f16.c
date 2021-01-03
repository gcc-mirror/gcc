/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rintp_f16_m_tied12:
**	frintp	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_UNIFORM_Z (rintp_f16_m_tied12, svfloat16_t,
		z0 = svrintp_f16_m (z0, p0, z0),
		z0 = svrintp_m (z0, p0, z0))

/*
** rintp_f16_m_tied1:
**	frintp	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rintp_f16_m_tied1, svfloat16_t,
		z0 = svrintp_f16_m (z0, p0, z1),
		z0 = svrintp_m (z0, p0, z1))

/*
** rintp_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	frintp	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_UNIFORM_Z (rintp_f16_m_tied2, svfloat16_t,
		z0 = svrintp_f16_m (z1, p0, z0),
		z0 = svrintp_m (z1, p0, z0))

/*
** rintp_f16_m_untied:
**	movprfx	z0, z2
**	frintp	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rintp_f16_m_untied, svfloat16_t,
		z0 = svrintp_f16_m (z2, p0, z1),
		z0 = svrintp_m (z2, p0, z1))

/*
** rintp_f16_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, \1\.h
**	frintp	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_UNIFORM_Z (rintp_f16_z_tied1, svfloat16_t,
		z0 = svrintp_f16_z (p0, z0),
		z0 = svrintp_z (p0, z0))

/*
** rintp_f16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	frintp	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rintp_f16_z_untied, svfloat16_t,
		z0 = svrintp_f16_z (p0, z1),
		z0 = svrintp_z (p0, z1))

/*
** rintp_f16_x_tied1:
**	frintp	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_UNIFORM_Z (rintp_f16_x_tied1, svfloat16_t,
		z0 = svrintp_f16_x (p0, z0),
		z0 = svrintp_x (p0, z0))

/*
** rintp_f16_x_untied:
**	movprfx	z0, z1
**	frintp	z0\.h, p0/m, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rintp_f16_x_untied, svfloat16_t,
		z0 = svrintp_f16_x (p0, z1),
		z0 = svrintp_x (p0, z1))

/*
** ptrue_rintp_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_rintp_f16_x_tied1, svfloat16_t,
		z0 = svrintp_f16_x (svptrue_b16 (), z0),
		z0 = svrintp_x (svptrue_b16 (), z0))

/*
** ptrue_rintp_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_rintp_f16_x_untied, svfloat16_t,
		z0 = svrintp_f16_x (svptrue_b16 (), z1),
		z0 = svrintp_x (svptrue_b16 (), z1))
