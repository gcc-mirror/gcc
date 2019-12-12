/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** minnmv_d0_f64_tied:
**	fminnmv	d0, p0, z0\.d
**	ret
*/
TEST_REDUCTION_D (minnmv_d0_f64_tied, float64_t, svfloat64_t,
		  d0 = svminnmv_f64 (p0, z0),
		  d0 = svminnmv (p0, z0))

/*
** minnmv_d0_f64_untied:
**	fminnmv	d0, p0, z1\.d
**	ret
*/
TEST_REDUCTION_D (minnmv_d0_f64_untied, float64_t, svfloat64_t,
		  d0 = svminnmv_f64 (p0, z1),
		  d0 = svminnmv (p0, z1))
