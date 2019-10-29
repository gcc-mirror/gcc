/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpeq_f16_tied:
**	fcmeq	p0\.h, p0/z, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpeq_f16_tied, svfloat16_t,
		p0 = svcmpeq_f16 (p0, z0, z1),
		p0 = svcmpeq (p0, z0, z1))

/*
** cmpeq_f16_untied:
**	fcmeq	p0\.h, p1/z, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpeq_f16_untied, svfloat16_t,
		p0 = svcmpeq_f16 (p1, z0, z1),
		p0 = svcmpeq (p1, z0, z1))

/*
** cmpeq_h4_f16:
**	mov	(z[0-9]+\.h), h4
**	fcmeq	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_ZD (cmpeq_h4_f16, svfloat16_t, float16_t,
		 p0 = svcmpeq_n_f16 (p1, z0, d4),
		 p0 = svcmpeq (p1, z0, d4))

/*
** cmpeq_0_f16:
**	fcmeq	p0\.h, p1/z, z0\.h, #0\.0
**	ret
*/
TEST_COMPARE_Z (cmpeq_0_f16, svfloat16_t,
		p0 = svcmpeq_n_f16 (p1, z0, 0),
		p0 = svcmpeq (p1, z0, 0))

/*
** cmpeq_1_f16:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
**	fcmeq	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpeq_1_f16, svfloat16_t,
		p0 = svcmpeq_n_f16 (p1, z0, 1),
		p0 = svcmpeq (p1, z0, 1))
