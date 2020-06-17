/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrnb_1_u64:
**	uqrshrnb	z0\.s, z0\.d, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_1_u64, svuint32_t, svuint64_t,
		    z0_res = svqrshrnb_n_u64 (z0, 1),
		    z0_res = svqrshrnb (z0, 1))

/*
** qrshrnb_2_u64:
**	uqrshrnb	z0\.s, z0\.d, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_2_u64, svuint32_t, svuint64_t,
		    z0_res = svqrshrnb_n_u64 (z0, 2),
		    z0_res = svqrshrnb (z0, 2))

/*
** qrshrnb_32_u64_tied1:
**	uqrshrnb	z0\.s, z0\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_32_u64_tied1, svuint32_t, svuint64_t,
		    z0_res = svqrshrnb_n_u64 (z0, 32),
		    z0_res = svqrshrnb (z0, 32))

/*
** qrshrnb_32_u64_untied:
**	uqrshrnb	z0\.s, z1\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_32_u64_untied, svuint32_t, svuint64_t,
		    z0_res = svqrshrnb_n_u64 (z1, 32),
		    z0_res = svqrshrnb (z1, 32))
