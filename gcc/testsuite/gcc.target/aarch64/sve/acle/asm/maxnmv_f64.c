/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** maxnmv_d0_f64_tied:
**	fmaxnmv	d0, p0, z0\.d
**	ret
*/
TEST_REDUCTION_D (maxnmv_d0_f64_tied, float64_t, svfloat64_t,
		  d0 = svmaxnmv_f64 (p0, z0),
		  d0 = svmaxnmv (p0, z0))

/*
** maxnmv_d0_f64_untied:
**	fmaxnmv	d0, p0, z1\.d
**	ret
*/
TEST_REDUCTION_D (maxnmv_d0_f64_untied, float64_t, svfloat64_t,
		  d0 = svmaxnmv_f64 (p0, z1),
		  d0 = svmaxnmv (p0, z1))
