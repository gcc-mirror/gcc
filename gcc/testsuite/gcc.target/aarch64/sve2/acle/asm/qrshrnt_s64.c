/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrnt_1_s64:
**	sqrshrnt	z0\.s, z4\.d, #1
**	ret
*/
TEST_DUAL_Z (qrshrnt_1_s64, svint32_t, svint64_t,
	     z0 = svqrshrnt_n_s64 (z0, z4, 1),
	     z0 = svqrshrnt (z0, z4, 1))

/*
** qrshrnt_2_s64:
**	sqrshrnt	z0\.s, z4\.d, #2
**	ret
*/
TEST_DUAL_Z (qrshrnt_2_s64, svint32_t, svint64_t,
	     z0 = svqrshrnt_n_s64 (z0, z4, 2),
	     z0 = svqrshrnt (z0, z4, 2))

/*
** qrshrnt_32_s64_tied1:
**	sqrshrnt	z0\.s, z4\.d, #32
**	ret
*/
TEST_DUAL_Z (qrshrnt_32_s64_tied1, svint32_t, svint64_t,
	     z0 = svqrshrnt_n_s64 (z0, z4, 32),
	     z0 = svqrshrnt (z0, z4, 32))

/*
** qrshrnt_32_s64_untied:
** (
**	mov	z0\.d, z1\.d
**	sqrshrnt	z0\.s, z4\.d, #32
** |
**	sqrshrnt	z1\.s, z4\.d, #32
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qrshrnt_32_s64_untied, svint32_t, svint64_t,
	     z0 = svqrshrnt_n_s64 (z1, z4, 32),
	     z0 = svqrshrnt (z1, z4, 32))
