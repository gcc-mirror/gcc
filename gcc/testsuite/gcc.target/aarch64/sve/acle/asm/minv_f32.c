/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** minv_d0_f32_tied:
**	fminv	s0, p0, z0\.s
**	ret
*/
TEST_REDUCTION_D (minv_d0_f32_tied, float32_t, svfloat32_t,
		  d0 = svminv_f32 (p0, z0),
		  d0 = svminv (p0, z0))

/*
** minv_d0_f32_untied:
**	fminv	s0, p0, z1\.s
**	ret
*/
TEST_REDUCTION_D (minv_d0_f32_untied, float32_t, svfloat32_t,
		  d0 = svminv_f32 (p0, z1),
		  d0 = svminv (p0, z1))
