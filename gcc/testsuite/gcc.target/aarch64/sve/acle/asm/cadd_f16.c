/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cadd_90_f16_m_tied1:
**	fcadd	z0\.h, p0/m, z0\.h, z1\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cadd_90_f16_m_tied1, svfloat16_t,
		z0 = svcadd_f16_m (p0, z0, z1, 90),
		z0 = svcadd_m (p0, z0, z1, 90))

/*
** cadd_90_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcadd	z0\.h, p0/m, z0\.h, \1\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cadd_90_f16_m_tied2, svfloat16_t,
		z0 = svcadd_f16_m (p0, z1, z0, 90),
		z0 = svcadd_m (p0, z1, z0, 90))

/*
** cadd_90_f16_m_untied:
**	movprfx	z0, z1
**	fcadd	z0\.h, p0/m, z0\.h, z2\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cadd_90_f16_m_untied, svfloat16_t,
		z0 = svcadd_f16_m (p0, z1, z2, 90),
		z0 = svcadd_m (p0, z1, z2, 90))

/*
** cadd_270_f16_m_tied1:
**	fcadd	z0\.h, p0/m, z0\.h, z1\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cadd_270_f16_m_tied1, svfloat16_t,
		z0 = svcadd_f16_m (p0, z0, z1, 270),
		z0 = svcadd_m (p0, z0, z1, 270))

/*
** cadd_270_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcadd	z0\.h, p0/m, z0\.h, \1\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cadd_270_f16_m_tied2, svfloat16_t,
		z0 = svcadd_f16_m (p0, z1, z0, 270),
		z0 = svcadd_m (p0, z1, z0, 270))

/*
** cadd_270_f16_m_untied:
**	movprfx	z0, z1
**	fcadd	z0\.h, p0/m, z0\.h, z2\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cadd_270_f16_m_untied, svfloat16_t,
		z0 = svcadd_f16_m (p0, z1, z2, 270),
		z0 = svcadd_m (p0, z1, z2, 270))

/*
** cadd_90_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fcadd	z0\.h, p0/m, z0\.h, z1\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cadd_90_f16_z_tied1, svfloat16_t,
		z0 = svcadd_f16_z (p0, z0, z1, 90),
		z0 = svcadd_z (p0, z0, z1, 90))

/*
** cadd_90_f16_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z1\.h
**	fcadd	z0\.h, p0/m, z0\.h, \1\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cadd_90_f16_z_tied2, svfloat16_t,
		z0 = svcadd_f16_z (p0, z1, z0, 90),
		z0 = svcadd_z (p0, z1, z0, 90))

/*
** cadd_90_f16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	fcadd	z0\.h, p0/m, z0\.h, z2\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cadd_90_f16_z_untied, svfloat16_t,
		z0 = svcadd_f16_z (p0, z1, z2, 90),
		z0 = svcadd_z (p0, z1, z2, 90))

/*
** cadd_270_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fcadd	z0\.h, p0/m, z0\.h, z1\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cadd_270_f16_z_tied1, svfloat16_t,
		z0 = svcadd_f16_z (p0, z0, z1, 270),
		z0 = svcadd_z (p0, z0, z1, 270))

/*
** cadd_270_f16_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z1\.h
**	fcadd	z0\.h, p0/m, z0\.h, \1\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cadd_270_f16_z_tied2, svfloat16_t,
		z0 = svcadd_f16_z (p0, z1, z0, 270),
		z0 = svcadd_z (p0, z1, z0, 270))

/*
** cadd_270_f16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	fcadd	z0\.h, p0/m, z0\.h, z2\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cadd_270_f16_z_untied, svfloat16_t,
		z0 = svcadd_f16_z (p0, z1, z2, 270),
		z0 = svcadd_z (p0, z1, z2, 270))

/*
** cadd_90_f16_x_tied1:
**	fcadd	z0\.h, p0/m, z0\.h, z1\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cadd_90_f16_x_tied1, svfloat16_t,
		z0 = svcadd_f16_x (p0, z0, z1, 90),
		z0 = svcadd_x (p0, z0, z1, 90))

/*
** cadd_90_f16_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcadd	z0\.h, p0/m, z0\.h, \1\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cadd_90_f16_x_tied2, svfloat16_t,
		z0 = svcadd_f16_x (p0, z1, z0, 90),
		z0 = svcadd_x (p0, z1, z0, 90))

/*
** cadd_90_f16_x_untied:
**	movprfx	z0, z1
**	fcadd	z0\.h, p0/m, z0\.h, z2\.h, #90
**	ret
*/
TEST_UNIFORM_Z (cadd_90_f16_x_untied, svfloat16_t,
		z0 = svcadd_f16_x (p0, z1, z2, 90),
		z0 = svcadd_x (p0, z1, z2, 90))

/*
** cadd_270_f16_x_tied1:
**	fcadd	z0\.h, p0/m, z0\.h, z1\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cadd_270_f16_x_tied1, svfloat16_t,
		z0 = svcadd_f16_x (p0, z0, z1, 270),
		z0 = svcadd_x (p0, z0, z1, 270))

/*
** cadd_270_f16_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fcadd	z0\.h, p0/m, z0\.h, \1\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cadd_270_f16_x_tied2, svfloat16_t,
		z0 = svcadd_f16_x (p0, z1, z0, 270),
		z0 = svcadd_x (p0, z1, z0, 270))

/*
** cadd_270_f16_x_untied:
**	movprfx	z0, z1
**	fcadd	z0\.h, p0/m, z0\.h, z2\.h, #270
**	ret
*/
TEST_UNIFORM_Z (cadd_270_f16_x_untied, svfloat16_t,
		z0 = svcadd_f16_x (p0, z1, z2, 270),
		z0 = svcadd_x (p0, z1, z2, 270))

/*
** ptrue_cadd_90_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cadd_90_f16_x_tied1, svfloat16_t,
		z0 = svcadd_f16_x (svptrue_b16 (), z0, z1, 90),
		z0 = svcadd_x (svptrue_b16 (), z0, z1, 90))

/*
** ptrue_cadd_90_f16_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cadd_90_f16_x_tied2, svfloat16_t,
		z0 = svcadd_f16_x (svptrue_b16 (), z1, z0, 90),
		z0 = svcadd_x (svptrue_b16 (), z1, z0, 90))

/*
** ptrue_cadd_90_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cadd_90_f16_x_untied, svfloat16_t,
		z0 = svcadd_f16_x (svptrue_b16 (), z1, z2, 90),
		z0 = svcadd_x (svptrue_b16 (), z1, z2, 90))

/*
** ptrue_cadd_270_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cadd_270_f16_x_tied1, svfloat16_t,
		z0 = svcadd_f16_x (svptrue_b16 (), z0, z1, 270),
		z0 = svcadd_x (svptrue_b16 (), z0, z1, 270))

/*
** ptrue_cadd_270_f16_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cadd_270_f16_x_tied2, svfloat16_t,
		z0 = svcadd_f16_x (svptrue_b16 (), z1, z0, 270),
		z0 = svcadd_x (svptrue_b16 (), z1, z0, 270))

/*
** ptrue_cadd_270_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_cadd_270_f16_x_untied, svfloat16_t,
		z0 = svcadd_f16_x (svptrue_b16 (), z1, z2, 270),
		z0 = svcadd_x (svptrue_b16 (), z1, z2, 270))
