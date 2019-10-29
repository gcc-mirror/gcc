/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** maxv_d0_f16_tied:
**	fmaxv	h0, p0, z0\.h
**	ret
*/
TEST_REDUCTION_D (maxv_d0_f16_tied, float16_t, svfloat16_t,
		  d0 = svmaxv_f16 (p0, z0),
		  d0 = svmaxv (p0, z0))

/*
** maxv_d0_f16_untied:
**	fmaxv	h0, p0, z1\.h
**	ret
*/
TEST_REDUCTION_D (maxv_d0_f16_untied, float16_t, svfloat16_t,
		  d0 = svmaxv_f16 (p0, z1),
		  d0 = svmaxv (p0, z1))
