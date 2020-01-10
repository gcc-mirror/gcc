/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** maxp_s32_m_tied1:
**	smaxp	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (maxp_s32_m_tied1, svint32_t,
		z0 = svmaxp_s32_m (p0, z0, z1),
		z0 = svmaxp_m (p0, z0, z1))

/*
** maxp_s32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	smaxp	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (maxp_s32_m_tied2, svint32_t,
		z0 = svmaxp_s32_m (p0, z1, z0),
		z0 = svmaxp_m (p0, z1, z0))

/*
** maxp_s32_m_untied:
**	movprfx	z0, z1
**	smaxp	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (maxp_s32_m_untied, svint32_t,
		z0 = svmaxp_s32_m (p0, z1, z2),
		z0 = svmaxp_m (p0, z1, z2))

/*
** maxp_s32_x_tied1:
**	smaxp	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (maxp_s32_x_tied1, svint32_t,
		z0 = svmaxp_s32_x (p0, z0, z1),
		z0 = svmaxp_x (p0, z0, z1))

/*
** maxp_s32_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	smaxp	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (maxp_s32_x_tied2, svint32_t,
		z0 = svmaxp_s32_x (p0, z1, z0),
		z0 = svmaxp_x (p0, z1, z0))

/*
** maxp_s32_x_untied:
**	movprfx	z0, z1
**	smaxp	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (maxp_s32_x_untied, svint32_t,
		z0 = svmaxp_s32_x (p0, z1, z2),
		z0 = svmaxp_x (p0, z1, z2))
