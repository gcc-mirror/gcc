/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrnb_1_s64:
**	sqshrnb	z0\.s, z0\.d, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_1_s64, svint32_t, svint64_t,
		    z0_res = svqshrnb_n_s64 (z0, 1),
		    z0_res = svqshrnb (z0, 1))

/*
** qshrnb_2_s64:
**	sqshrnb	z0\.s, z0\.d, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_2_s64, svint32_t, svint64_t,
		    z0_res = svqshrnb_n_s64 (z0, 2),
		    z0_res = svqshrnb (z0, 2))

/*
** qshrnb_32_s64_tied1:
**	sqshrnb	z0\.s, z0\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_32_s64_tied1, svint32_t, svint64_t,
		    z0_res = svqshrnb_n_s64 (z0, 32),
		    z0_res = svqshrnb (z0, 32))

/*
** qshrnb_32_s64_untied:
**	sqshrnb	z0\.s, z1\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_32_s64_untied, svint32_t, svint64_t,
		    z0_res = svqshrnb_n_s64 (z1, 32),
		    z0_res = svqshrnb (z1, 32))
