/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addv_d0_f32_tied:
**	faddv	s0, p0, z0\.s
**	ret
*/
TEST_REDUCTION_D (addv_d0_f32_tied, float32_t, svfloat32_t,
		  d0 = svaddv_f32 (p0, z0),
		  d0 = svaddv (p0, z0))

/*
** addv_d0_f32_untied:
**	faddv	s0, p0, z1\.s
**	ret
*/
TEST_REDUCTION_D (addv_d0_f32_untied, float32_t, svfloat32_t,
		  d0 = svaddv_f32 (p0, z1),
		  d0 = svaddv (p0, z1))
