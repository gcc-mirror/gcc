/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mul_lane_0_f64_tied1:
**	fmul	z0\.d, z0\.d, z1\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_0_f64_tied1, svfloat64_t,
		z0 = svmul_lane_f64 (z0, z1, 0),
		z0 = svmul_lane (z0, z1, 0))

/*
** mul_lane_0_f64_tied2:
**	fmul	z0\.d, z1\.d, z0\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_0_f64_tied2, svfloat64_t,
		z0 = svmul_lane_f64 (z1, z0, 0),
		z0 = svmul_lane (z1, z0, 0))

/*
** mul_lane_0_f64_untied:
**	fmul	z0\.d, z1\.d, z2\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_0_f64_untied, svfloat64_t,
		z0 = svmul_lane_f64 (z1, z2, 0),
		z0 = svmul_lane (z1, z2, 0))

/*
** mul_lane_1_f64:
**	fmul	z0\.d, z1\.d, z2\.d\[1\]
**	ret
*/
TEST_UNIFORM_Z (mul_lane_1_f64, svfloat64_t,
		z0 = svmul_lane_f64 (z1, z2, 1),
		z0 = svmul_lane (z1, z2, 1))

/*
** mul_lane_z7_f64:
**	fmul	z0\.d, z1\.d, z7\.d\[1\]
**	ret
*/
TEST_DUAL_Z (mul_lane_z7_f64, svfloat64_t, svfloat64_t,
	     z0 = svmul_lane_f64 (z1, z7, 1),
	     z0 = svmul_lane (z1, z7, 1))

/*
** mul_lane_z15_f64:
**	str	d15, \[sp, -16\]!
**	fmul	z0\.d, z1\.d, z15\.d\[1\]
**	ldr	d15, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (mul_lane_z15_f64, svfloat64_t, svfloat64_t, z15,
		    z0 = svmul_lane_f64 (z1, z15, 1),
		    z0 = svmul_lane (z1, z15, 1))

/*
** mul_lane_z16_f64:
**	mov	(z[0-9]|z1[0-5])\.d, z16\.d
**	fmul	z0\.d, z1\.d, \1\.d\[1\]
**	ret
*/
TEST_DUAL_LANE_REG (mul_lane_z16_f64, svfloat64_t, svfloat64_t, z16,
		    z0 = svmul_lane_f64 (z1, z16, 1),
		    z0 = svmul_lane (z1, z16, 1))
