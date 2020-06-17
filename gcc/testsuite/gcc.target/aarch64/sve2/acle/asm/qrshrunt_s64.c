/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrunt_1_s64:
**	sqrshrunt	z0\.s, z4\.d, #1
**	ret
*/
TEST_DUAL_Z (qrshrunt_1_s64, svuint32_t, svint64_t,
	     z0 = svqrshrunt_n_s64 (z0, z4, 1),
	     z0 = svqrshrunt (z0, z4, 1))

/*
** qrshrunt_2_s64:
**	sqrshrunt	z0\.s, z4\.d, #2
**	ret
*/
TEST_DUAL_Z (qrshrunt_2_s64, svuint32_t, svint64_t,
	     z0 = svqrshrunt_n_s64 (z0, z4, 2),
	     z0 = svqrshrunt (z0, z4, 2))

/*
** qrshrunt_32_s64_tied1:
**	sqrshrunt	z0\.s, z4\.d, #32
**	ret
*/
TEST_DUAL_Z (qrshrunt_32_s64_tied1, svuint32_t, svint64_t,
	     z0 = svqrshrunt_n_s64 (z0, z4, 32),
	     z0 = svqrshrunt (z0, z4, 32))

/*
** qrshrunt_32_s64_untied:
** (
**	mov	z0\.d, z1\.d
**	sqrshrunt	z0\.s, z4\.d, #32
** |
**	sqrshrunt	z1\.s, z4\.d, #32
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qrshrunt_32_s64_untied, svuint32_t, svint64_t,
	     z0 = svqrshrunt_n_s64 (z1, z4, 32),
	     z0 = svqrshrunt (z1, z4, 32))
