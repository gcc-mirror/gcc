/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrnt_1_u64:
**	uqrshrnt	z0\.s, z4\.d, #1
**	ret
*/
TEST_DUAL_Z (qrshrnt_1_u64, svuint32_t, svuint64_t,
	     z0 = svqrshrnt_n_u64 (z0, z4, 1),
	     z0 = svqrshrnt (z0, z4, 1))

/*
** qrshrnt_2_u64:
**	uqrshrnt	z0\.s, z4\.d, #2
**	ret
*/
TEST_DUAL_Z (qrshrnt_2_u64, svuint32_t, svuint64_t,
	     z0 = svqrshrnt_n_u64 (z0, z4, 2),
	     z0 = svqrshrnt (z0, z4, 2))

/*
** qrshrnt_32_u64_tied1:
**	uqrshrnt	z0\.s, z4\.d, #32
**	ret
*/
TEST_DUAL_Z (qrshrnt_32_u64_tied1, svuint32_t, svuint64_t,
	     z0 = svqrshrnt_n_u64 (z0, z4, 32),
	     z0 = svqrshrnt (z0, z4, 32))

/*
** qrshrnt_32_u64_untied:
** (
**	mov	z0\.d, z1\.d
**	uqrshrnt	z0\.s, z4\.d, #32
** |
**	uqrshrnt	z1\.s, z4\.d, #32
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qrshrnt_32_u64_untied, svuint32_t, svuint64_t,
	     z0 = svqrshrnt_n_u64 (z1, z4, 32),
	     z0 = svqrshrnt (z1, z4, 32))
