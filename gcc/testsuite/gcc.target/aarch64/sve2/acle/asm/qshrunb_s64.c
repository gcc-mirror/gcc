/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrunb_1_s64:
**	sqshrunb	z0\.s, z0\.d, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrunb_1_s64, svuint32_t, svint64_t,
		    z0_res = svqshrunb_n_s64 (z0, 1),
		    z0_res = svqshrunb (z0, 1))

/*
** qshrunb_2_s64:
**	sqshrunb	z0\.s, z0\.d, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrunb_2_s64, svuint32_t, svint64_t,
		    z0_res = svqshrunb_n_s64 (z0, 2),
		    z0_res = svqshrunb (z0, 2))

/*
** qshrunb_32_s64_tied1:
**	sqshrunb	z0\.s, z0\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrunb_32_s64_tied1, svuint32_t, svint64_t,
		    z0_res = svqshrunb_n_s64 (z0, 32),
		    z0_res = svqshrunb (z0, 32))

/*
** qshrunb_32_s64_untied:
**	sqshrunb	z0\.s, z1\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrunb_32_s64_untied, svuint32_t, svint64_t,
		    z0_res = svqshrunb_n_s64 (z1, 32),
		    z0_res = svqshrunb (z1, 32))
