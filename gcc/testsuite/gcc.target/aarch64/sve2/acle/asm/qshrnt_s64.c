/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrnt_1_s64:
**	sqshrnt	z0\.s, z4\.d, #1
**	ret
*/
TEST_DUAL_Z (qshrnt_1_s64, svint32_t, svint64_t,
	     z0 = svqshrnt_n_s64 (z0, z4, 1),
	     z0 = svqshrnt (z0, z4, 1))

/*
** qshrnt_2_s64:
**	sqshrnt	z0\.s, z4\.d, #2
**	ret
*/
TEST_DUAL_Z (qshrnt_2_s64, svint32_t, svint64_t,
	     z0 = svqshrnt_n_s64 (z0, z4, 2),
	     z0 = svqshrnt (z0, z4, 2))

/*
** qshrnt_32_s64_tied1:
**	sqshrnt	z0\.s, z4\.d, #32
**	ret
*/
TEST_DUAL_Z (qshrnt_32_s64_tied1, svint32_t, svint64_t,
	     z0 = svqshrnt_n_s64 (z0, z4, 32),
	     z0 = svqshrnt (z0, z4, 32))

/*
** qshrnt_32_s64_untied:
** (
**	mov	z0\.d, z1\.d
**	sqshrnt	z0\.s, z4\.d, #32
** |
**	sqshrnt	z1\.s, z4\.d, #32
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qshrnt_32_s64_untied, svint32_t, svint64_t,
	     z0 = svqshrnt_n_s64 (z1, z4, 32),
	     z0 = svqshrnt (z1, z4, 32))
