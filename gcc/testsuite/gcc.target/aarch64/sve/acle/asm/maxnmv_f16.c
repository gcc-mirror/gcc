/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** maxnmv_d0_f16_tied:
**	fmaxnmv	h0, p0, z0\.h
**	ret
*/
TEST_REDUCTION_D (maxnmv_d0_f16_tied, float16_t, svfloat16_t,
		  d0 = svmaxnmv_f16 (p0, z0),
		  d0 = svmaxnmv (p0, z0))

/*
** maxnmv_d0_f16_untied:
**	fmaxnmv	h0, p0, z1\.h
**	ret
*/
TEST_REDUCTION_D (maxnmv_d0_f16_untied, float16_t, svfloat16_t,
		  d0 = svmaxnmv_f16 (p0, z1),
		  d0 = svmaxnmv (p0, z1))
