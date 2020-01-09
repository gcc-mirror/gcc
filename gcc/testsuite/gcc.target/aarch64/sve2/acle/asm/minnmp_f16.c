/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** minnmp_f16_m_tied1:
**	fminnmp	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (minnmp_f16_m_tied1, svfloat16_t,
		z0 = svminnmp_f16_m (p0, z0, z1),
		z0 = svminnmp_m (p0, z0, z1))

/*
** minnmp_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fminnmp	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (minnmp_f16_m_tied2, svfloat16_t,
		z0 = svminnmp_f16_m (p0, z1, z0),
		z0 = svminnmp_m (p0, z1, z0))

/*
** minnmp_f16_m_untied:
**	movprfx	z0, z1
**	fminnmp	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (minnmp_f16_m_untied, svfloat16_t,
		z0 = svminnmp_f16_m (p0, z1, z2),
		z0 = svminnmp_m (p0, z1, z2))

/*
** minnmp_f16_x_tied1:
**	fminnmp	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (minnmp_f16_x_tied1, svfloat16_t,
		z0 = svminnmp_f16_x (p0, z0, z1),
		z0 = svminnmp_x (p0, z0, z1))

/*
** minnmp_f16_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fminnmp	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (minnmp_f16_x_tied2, svfloat16_t,
		z0 = svminnmp_f16_x (p0, z1, z0),
		z0 = svminnmp_x (p0, z1, z0))

/*
** minnmp_f16_x_untied:
**	movprfx	z0, z1
**	fminnmp	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (minnmp_f16_x_untied, svfloat16_t,
		z0 = svminnmp_f16_x (p0, z1, z2),
		z0 = svminnmp_x (p0, z1, z2))

/*
** ptrue_minnmp_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_minnmp_f16_x_tied1, svfloat16_t,
		z0 = svminnmp_f16_x (svptrue_b16 (), z0, z1),
		z0 = svminnmp_x (svptrue_b16 (), z0, z1))

/*
** ptrue_minnmp_f16_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_minnmp_f16_x_tied2, svfloat16_t,
		z0 = svminnmp_f16_x (svptrue_b16 (), z1, z0),
		z0 = svminnmp_x (svptrue_b16 (), z1, z0))

/*
** ptrue_minnmp_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_minnmp_f16_x_untied, svfloat16_t,
		z0 = svminnmp_f16_x (svptrue_b16 (), z1, z2),
		z0 = svminnmp_x (svptrue_b16 (), z1, z2))
