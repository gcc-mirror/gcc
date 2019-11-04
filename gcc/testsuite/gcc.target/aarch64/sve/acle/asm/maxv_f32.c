/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** maxv_d0_f32_tied:
**	fmaxv	s0, p0, z0\.s
**	ret
*/
TEST_REDUCTION_D (maxv_d0_f32_tied, float32_t, svfloat32_t,
		  d0 = svmaxv_f32 (p0, z0),
		  d0 = svmaxv (p0, z0))

/*
** maxv_d0_f32_untied:
**	fmaxv	s0, p0, z1\.s
**	ret
*/
TEST_REDUCTION_D (maxv_d0_f32_untied, float32_t, svfloat32_t,
		  d0 = svmaxv_f32 (p0, z1),
		  d0 = svmaxv (p0, z1))
