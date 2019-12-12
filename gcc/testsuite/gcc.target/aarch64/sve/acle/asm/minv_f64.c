/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** minv_d0_f64_tied:
**	fminv	d0, p0, z0\.d
**	ret
*/
TEST_REDUCTION_D (minv_d0_f64_tied, float64_t, svfloat64_t,
		  d0 = svminv_f64 (p0, z0),
		  d0 = svminv (p0, z0))

/*
** minv_d0_f64_untied:
**	fminv	d0, p0, z1\.d
**	ret
*/
TEST_REDUCTION_D (minv_d0_f64_untied, float64_t, svfloat64_t,
		  d0 = svminv_f64 (p0, z1),
		  d0 = svminv (p0, z1))
