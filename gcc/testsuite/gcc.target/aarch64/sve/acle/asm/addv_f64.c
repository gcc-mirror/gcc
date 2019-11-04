/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addv_d0_f64_tied:
**	faddv	d0, p0, z0\.d
**	ret
*/
TEST_REDUCTION_D (addv_d0_f64_tied, float64_t, svfloat64_t,
		  d0 = svaddv_f64 (p0, z0),
		  d0 = svaddv (p0, z0))

/*
** addv_d0_f64_untied:
**	faddv	d0, p0, z1\.d
**	ret
*/
TEST_REDUCTION_D (addv_d0_f64_untied, float64_t, svfloat64_t,
		  d0 = svaddv_f64 (p0, z1),
		  d0 = svaddv (p0, z1))
