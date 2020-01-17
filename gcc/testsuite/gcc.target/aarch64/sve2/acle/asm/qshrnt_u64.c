/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrnt_1_u64:
**	uqshrnt	z0\.s, z4\.d, #1
**	ret
*/
TEST_DUAL_Z (qshrnt_1_u64, svuint32_t, svuint64_t,
	     z0 = svqshrnt_n_u64 (z0, z4, 1),
	     z0 = svqshrnt (z0, z4, 1))

/*
** qshrnt_2_u64:
**	uqshrnt	z0\.s, z4\.d, #2
**	ret
*/
TEST_DUAL_Z (qshrnt_2_u64, svuint32_t, svuint64_t,
	     z0 = svqshrnt_n_u64 (z0, z4, 2),
	     z0 = svqshrnt (z0, z4, 2))

/*
** qshrnt_32_u64_tied1:
**	uqshrnt	z0\.s, z4\.d, #32
**	ret
*/
TEST_DUAL_Z (qshrnt_32_u64_tied1, svuint32_t, svuint64_t,
	     z0 = svqshrnt_n_u64 (z0, z4, 32),
	     z0 = svqshrnt (z0, z4, 32))

/*
** qshrnt_32_u64_untied:
** (
**	mov	z0\.d, z1\.d
**	uqshrnt	z0\.s, z4\.d, #32
** |
**	uqshrnt	z1\.s, z4\.d, #32
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qshrnt_32_u64_untied, svuint32_t, svuint64_t,
	     z0 = svqshrnt_n_u64 (z1, z4, 32),
	     z0 = svqshrnt (z1, z4, 32))
