/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrunb_1_s64:
**	sqrshrunb	z0\.s, z0\.d, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrunb_1_s64, svuint32_t, svint64_t,
		    z0_res = svqrshrunb_n_s64 (z0, 1),
		    z0_res = svqrshrunb (z0, 1))

/*
** qrshrunb_2_s64:
**	sqrshrunb	z0\.s, z0\.d, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrunb_2_s64, svuint32_t, svint64_t,
		    z0_res = svqrshrunb_n_s64 (z0, 2),
		    z0_res = svqrshrunb (z0, 2))

/*
** qrshrunb_32_s64_tied1:
**	sqrshrunb	z0\.s, z0\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrunb_32_s64_tied1, svuint32_t, svint64_t,
		    z0_res = svqrshrunb_n_s64 (z0, 32),
		    z0_res = svqrshrunb (z0, 32))

/*
** qrshrunb_32_s64_untied:
**	sqrshrunb	z0\.s, z1\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrunb_32_s64_untied, svuint32_t, svint64_t,
		    z0_res = svqrshrunb_n_s64 (z1, 32),
		    z0_res = svqrshrunb (z1, 32))
