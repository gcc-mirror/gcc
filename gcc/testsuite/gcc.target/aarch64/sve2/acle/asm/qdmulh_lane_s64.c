/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdmulh_lane_0_s64_tied1:
**	sqdmulh	z0\.d, z0\.d, z1\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_0_s64_tied1, svint64_t,
		z0 = svqdmulh_lane_s64 (z0, z1, 0),
		z0 = svqdmulh_lane (z0, z1, 0))

/*
** qdmulh_lane_0_s64_tied2:
**	sqdmulh	z0\.d, z1\.d, z0\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_0_s64_tied2, svint64_t,
		z0 = svqdmulh_lane_s64 (z1, z0, 0),
		z0 = svqdmulh_lane (z1, z0, 0))

/*
** qdmulh_lane_0_s64_untied:
**	sqdmulh	z0\.d, z1\.d, z2\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_0_s64_untied, svint64_t,
		z0 = svqdmulh_lane_s64 (z1, z2, 0),
		z0 = svqdmulh_lane (z1, z2, 0))

/*
** qdmulh_lane_1_s64:
**	sqdmulh	z0\.d, z1\.d, z2\.d\[1\]
**	ret
*/
TEST_UNIFORM_Z (qdmulh_lane_1_s64, svint64_t,
		z0 = svqdmulh_lane_s64 (z1, z2, 1),
		z0 = svqdmulh_lane (z1, z2, 1))

/*
** qdmulh_lane_z15_s64:
**	str	d15, \[sp, -16\]!
**	sqdmulh	z0\.d, z1\.d, z15\.d\[1\]
**	ldr	d15, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (qdmulh_lane_z15_s64, svint64_t, svint64_t, z15,
		    z0 = svqdmulh_lane_s64 (z1, z15, 1),
		    z0 = svqdmulh_lane (z1, z15, 1))

/*
** qdmulh_lane_z16_s64:
**	mov	(z[0-7])\.d, z16\.d
**	sqdmulh	z0\.d, z1\.d, \1\.d\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (qdmulh_lane_z16_s64, svint64_t, svint64_t, z16,
		    z0 = svqdmulh_lane_s64 (z1, z16, 1),
		    z0 = svqdmulh_lane (z1, z16, 1))
