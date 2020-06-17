/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** maxp_s16_m_tied1:
**	smaxp	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (maxp_s16_m_tied1, svint16_t,
		z0 = svmaxp_s16_m (p0, z0, z1),
		z0 = svmaxp_m (p0, z0, z1))

/*
** maxp_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	smaxp	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (maxp_s16_m_tied2, svint16_t,
		z0 = svmaxp_s16_m (p0, z1, z0),
		z0 = svmaxp_m (p0, z1, z0))

/*
** maxp_s16_m_untied:
**	movprfx	z0, z1
**	smaxp	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (maxp_s16_m_untied, svint16_t,
		z0 = svmaxp_s16_m (p0, z1, z2),
		z0 = svmaxp_m (p0, z1, z2))

/*
** maxp_s16_x_tied1:
**	smaxp	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (maxp_s16_x_tied1, svint16_t,
		z0 = svmaxp_s16_x (p0, z0, z1),
		z0 = svmaxp_x (p0, z0, z1))

/*
** maxp_s16_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	smaxp	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (maxp_s16_x_tied2, svint16_t,
		z0 = svmaxp_s16_x (p0, z1, z0),
		z0 = svmaxp_x (p0, z1, z0))

/*
** maxp_s16_x_untied:
**	movprfx	z0, z1
**	smaxp	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (maxp_s16_x_untied, svint16_t,
		z0 = svmaxp_s16_x (p0, z1, z2),
		z0 = svmaxp_x (p0, z1, z2))
