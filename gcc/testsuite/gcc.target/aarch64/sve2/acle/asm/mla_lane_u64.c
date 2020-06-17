/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mla_lane_0_u64_tied1:
**	mla	z0\.d, z1\.d, z2\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_0_u64_tied1, svuint64_t,
		z0 = svmla_lane_u64 (z0, z1, z2, 0),
		z0 = svmla_lane (z0, z1, z2, 0))

/*
** mla_lane_0_u64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	mla	z0\.d, \1, z2\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_0_u64_tied2, svuint64_t,
		z0 = svmla_lane_u64 (z1, z0, z2, 0),
		z0 = svmla_lane (z1, z0, z2, 0))

/*
** mla_lane_0_u64_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	mla	z0\.d, z2\.d, \1\[0\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_0_u64_tied3, svuint64_t,
		z0 = svmla_lane_u64 (z1, z2, z0, 0),
		z0 = svmla_lane (z1, z2, z0, 0))

/*
** mla_lane_0_u64_untied:
**	movprfx	z0, z1
**	mla	z0\.d, z2\.d, z3\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_0_u64_untied, svuint64_t,
		z0 = svmla_lane_u64 (z1, z2, z3, 0),
		z0 = svmla_lane (z1, z2, z3, 0))

/*
** mla_lane_1_u64:
**	mla	z0\.d, z1\.d, z2\.d\[1\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_1_u64, svuint64_t,
		z0 = svmla_lane_u64 (z0, z1, z2, 1),
		z0 = svmla_lane (z0, z1, z2, 1))

/*
** mla_lane_z15_u64:
**	str	d15, \[sp, -16\]!
**	mla	z0\.d, z1\.d, z15\.d\[1\]
**	ldr	d15, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (mla_lane_z15_u64, svuint64_t, svuint64_t, z15,
		    z0 = svmla_lane_u64 (z0, z1, z15, 1),
		    z0 = svmla_lane (z0, z1, z15, 1))

/*
** mla_lane_z16_u64:
**	mov	(z[0-7])\.d, z16\.d
**	mla	z0\.d, z1\.d, \1\.d\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (mla_lane_z16_u64, svuint64_t, svuint64_t, z16,
		    z0 = svmla_lane_u64 (z0, z1, z16, 1),
		    z0 = svmla_lane (z0, z1, z16, 1))
