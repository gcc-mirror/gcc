/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dupq_lane_0_f64_tied:
**	dup	z0\.q, z0\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dupq_lane_0_f64_tied, svfloat64_t,
		z0 = svdupq_lane_f64 (z0, 0),
		z0 = svdupq_lane (z0, 0))

/*
** dupq_lane_0_f64_untied:
**	dup	z0\.q, z1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dupq_lane_0_f64_untied, svfloat64_t,
		z0 = svdupq_lane_f64 (z1, 0),
		z0 = svdupq_lane (z1, 0))

/*
** dupq_lane_1_f64:
**	dup	z0\.q, z0\.q\[1\]
**	ret
*/
TEST_UNIFORM_Z (dupq_lane_1_f64, svfloat64_t,
		z0 = svdupq_lane_f64 (z0, 1),
		z0 = svdupq_lane (z0, 1))

/*
** dupq_lane_2_f64:
**	dup	z0\.q, z0\.q\[2\]
**	ret
*/
TEST_UNIFORM_Z (dupq_lane_2_f64, svfloat64_t,
		z0 = svdupq_lane_f64 (z0, 2),
		z0 = svdupq_lane (z0, 2))

/*
** dupq_lane_3_f64:
**	dup	z0\.q, z0\.q\[3\]
**	ret
*/
TEST_UNIFORM_Z (dupq_lane_3_f64, svfloat64_t,
		z0 = svdupq_lane_f64 (z0, 3),
		z0 = svdupq_lane (z0, 3))
