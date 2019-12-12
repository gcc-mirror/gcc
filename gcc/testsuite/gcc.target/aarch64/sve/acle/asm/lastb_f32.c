/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lastb_d0_f32_tied:
**	lastb	s0, p0, z0\.s
**	ret
*/
TEST_REDUCTION_D (lastb_d0_f32_tied, float32_t, svfloat32_t,
		  d0 = svlastb_f32 (p0, z0),
		  d0 = svlastb (p0, z0))

/*
** lastb_d0_f32_untied:
**	lastb	s0, p0, z1\.s
**	ret
*/
TEST_REDUCTION_D (lastb_d0_f32_untied, float32_t, svfloat32_t,
		  d0 = svlastb_f32 (p0, z1),
		  d0 = svlastb (p0, z1))
