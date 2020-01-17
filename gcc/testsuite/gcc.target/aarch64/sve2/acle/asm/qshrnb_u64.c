/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrnb_1_u64:
**	uqshrnb	z0\.s, z0\.d, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_1_u64, svuint32_t, svuint64_t,
		    z0_res = svqshrnb_n_u64 (z0, 1),
		    z0_res = svqshrnb (z0, 1))

/*
** qshrnb_2_u64:
**	uqshrnb	z0\.s, z0\.d, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_2_u64, svuint32_t, svuint64_t,
		    z0_res = svqshrnb_n_u64 (z0, 2),
		    z0_res = svqshrnb (z0, 2))

/*
** qshrnb_32_u64_tied1:
**	uqshrnb	z0\.s, z0\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_32_u64_tied1, svuint32_t, svuint64_t,
		    z0_res = svqshrnb_n_u64 (z0, 32),
		    z0_res = svqshrnb (z0, 32))

/*
** qshrnb_32_u64_untied:
**	uqshrnb	z0\.s, z1\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_32_u64_untied, svuint32_t, svuint64_t,
		    z0_res = svqshrnb_n_u64 (z1, 32),
		    z0_res = svqshrnb (z1, 32))
