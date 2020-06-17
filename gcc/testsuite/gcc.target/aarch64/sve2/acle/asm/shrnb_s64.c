/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shrnb_1_s64:
**	shrnb	z0\.s, z0\.d, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_1_s64, svint32_t, svint64_t,
		    z0_res = svshrnb_n_s64 (z0, 1),
		    z0_res = svshrnb (z0, 1))

/*
** shrnb_2_s64:
**	shrnb	z0\.s, z0\.d, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_2_s64, svint32_t, svint64_t,
		    z0_res = svshrnb_n_s64 (z0, 2),
		    z0_res = svshrnb (z0, 2))

/*
** shrnb_32_s64_tied1:
**	shrnb	z0\.s, z0\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_32_s64_tied1, svint32_t, svint64_t,
		    z0_res = svshrnb_n_s64 (z0, 32),
		    z0_res = svshrnb (z0, 32))

/*
** shrnb_32_s64_untied:
**	shrnb	z0\.s, z1\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_32_s64_untied, svint32_t, svint64_t,
		    z0_res = svshrnb_n_s64 (z1, 32),
		    z0_res = svshrnb (z1, 32))
