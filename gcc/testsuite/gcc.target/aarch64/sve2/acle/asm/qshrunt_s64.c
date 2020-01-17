/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrunt_1_s64:
**	sqshrunt	z0\.s, z4\.d, #1
**	ret
*/
TEST_DUAL_Z (qshrunt_1_s64, svuint32_t, svint64_t,
	     z0 = svqshrunt_n_s64 (z0, z4, 1),
	     z0 = svqshrunt (z0, z4, 1))

/*
** qshrunt_2_s64:
**	sqshrunt	z0\.s, z4\.d, #2
**	ret
*/
TEST_DUAL_Z (qshrunt_2_s64, svuint32_t, svint64_t,
	     z0 = svqshrunt_n_s64 (z0, z4, 2),
	     z0 = svqshrunt (z0, z4, 2))

/*
** qshrunt_32_s64_tied1:
**	sqshrunt	z0\.s, z4\.d, #32
**	ret
*/
TEST_DUAL_Z (qshrunt_32_s64_tied1, svuint32_t, svint64_t,
	     z0 = svqshrunt_n_s64 (z0, z4, 32),
	     z0 = svqshrunt (z0, z4, 32))

/*
** qshrunt_32_s64_untied:
** (
**	mov	z0\.d, z1\.d
**	sqshrunt	z0\.s, z4\.d, #32
** |
**	sqshrunt	z1\.s, z4\.d, #32
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qshrunt_32_s64_untied, svuint32_t, svint64_t,
	     z0 = svqshrunt_n_s64 (z1, z4, 32),
	     z0 = svqshrunt (z1, z4, 32))
