/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrnb_1_s64:
**	sqrshrnb	z0\.s, z0\.d, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_1_s64, svint32_t, svint64_t,
		    z0_res = svqrshrnb_n_s64 (z0, 1),
		    z0_res = svqrshrnb (z0, 1))

/*
** qrshrnb_2_s64:
**	sqrshrnb	z0\.s, z0\.d, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_2_s64, svint32_t, svint64_t,
		    z0_res = svqrshrnb_n_s64 (z0, 2),
		    z0_res = svqrshrnb (z0, 2))

/*
** qrshrnb_32_s64_tied1:
**	sqrshrnb	z0\.s, z0\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_32_s64_tied1, svint32_t, svint64_t,
		    z0_res = svqrshrnb_n_s64 (z0, 32),
		    z0_res = svqrshrnb (z0, 32))

/*
** qrshrnb_32_s64_untied:
**	sqrshrnb	z0\.s, z1\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_32_s64_untied, svint32_t, svint64_t,
		    z0_res = svqrshrnb_n_s64 (z1, 32),
		    z0_res = svqrshrnb (z1, 32))
