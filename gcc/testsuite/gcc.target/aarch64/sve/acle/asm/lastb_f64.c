/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lastb_d0_f64_tied:
**	lastb	d0, p0, z0\.d
**	ret
*/
TEST_REDUCTION_D (lastb_d0_f64_tied, float64_t, svfloat64_t,
		  d0 = svlastb_f64 (p0, z0),
		  d0 = svlastb (p0, z0))

/*
** lastb_d0_f64_untied:
**	lastb	d0, p0, z1\.d
**	ret
*/
TEST_REDUCTION_D (lastb_d0_f64_untied, float64_t, svfloat64_t,
		  d0 = svlastb_f64 (p0, z1),
		  d0 = svlastb (p0, z1))
