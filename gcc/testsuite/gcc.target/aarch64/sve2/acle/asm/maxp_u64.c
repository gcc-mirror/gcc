/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** maxp_u64_m_tied1:
**	umaxp	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (maxp_u64_m_tied1, svuint64_t,
		z0 = svmaxp_u64_m (p0, z0, z1),
		z0 = svmaxp_m (p0, z0, z1))

/*
** maxp_u64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	umaxp	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (maxp_u64_m_tied2, svuint64_t,
		z0 = svmaxp_u64_m (p0, z1, z0),
		z0 = svmaxp_m (p0, z1, z0))

/*
** maxp_u64_m_untied:
**	movprfx	z0, z1
**	umaxp	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (maxp_u64_m_untied, svuint64_t,
		z0 = svmaxp_u64_m (p0, z1, z2),
		z0 = svmaxp_m (p0, z1, z2))

/*
** maxp_u64_x_tied1:
**	umaxp	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (maxp_u64_x_tied1, svuint64_t,
		z0 = svmaxp_u64_x (p0, z0, z1),
		z0 = svmaxp_x (p0, z0, z1))

/*
** maxp_u64_x_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	umaxp	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (maxp_u64_x_tied2, svuint64_t,
		z0 = svmaxp_u64_x (p0, z1, z0),
		z0 = svmaxp_x (p0, z1, z0))

/*
** maxp_u64_x_untied:
**	movprfx	z0, z1
**	umaxp	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (maxp_u64_x_untied, svuint64_t,
		z0 = svmaxp_u64_x (p0, z1, z2),
		z0 = svmaxp_x (p0, z1, z2))
