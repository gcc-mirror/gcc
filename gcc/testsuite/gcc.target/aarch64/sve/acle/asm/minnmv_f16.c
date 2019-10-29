/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** minnmv_d0_f16_tied:
**	fminnmv	h0, p0, z0\.h
**	ret
*/
TEST_REDUCTION_D (minnmv_d0_f16_tied, float16_t, svfloat16_t,
		  d0 = svminnmv_f16 (p0, z0),
		  d0 = svminnmv (p0, z0))

/*
** minnmv_d0_f16_untied:
**	fminnmv	h0, p0, z1\.h
**	ret
*/
TEST_REDUCTION_D (minnmv_d0_f16_untied, float16_t, svfloat16_t,
		  d0 = svminnmv_f16 (p0, z1),
		  d0 = svminnmv (p0, z1))
