/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lasta_d0_f64_tied:
**	lasta	d0, p0, z0\.d
**	ret
*/
TEST_REDUCTION_D (lasta_d0_f64_tied, float64_t, svfloat64_t,
		  d0 = svlasta_f64 (p0, z0),
		  d0 = svlasta (p0, z0))

/*
** lasta_d0_f64_untied:
**	lasta	d0, p0, z1\.d
**	ret
*/
TEST_REDUCTION_D (lasta_d0_f64_untied, float64_t, svfloat64_t,
		  d0 = svlasta_f64 (p0, z1),
		  d0 = svlasta (p0, z1))
