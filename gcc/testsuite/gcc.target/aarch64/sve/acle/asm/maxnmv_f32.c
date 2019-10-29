/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** maxnmv_d0_f32_tied:
**	fmaxnmv	s0, p0, z0\.s
**	ret
*/
TEST_REDUCTION_D (maxnmv_d0_f32_tied, float32_t, svfloat32_t,
		  d0 = svmaxnmv_f32 (p0, z0),
		  d0 = svmaxnmv (p0, z0))

/*
** maxnmv_d0_f32_untied:
**	fmaxnmv	s0, p0, z1\.s
**	ret
*/
TEST_REDUCTION_D (maxnmv_d0_f32_untied, float32_t, svfloat32_t,
		  d0 = svmaxnmv_f32 (p0, z1),
		  d0 = svmaxnmv (p0, z1))
