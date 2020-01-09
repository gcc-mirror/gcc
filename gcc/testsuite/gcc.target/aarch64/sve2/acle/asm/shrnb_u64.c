/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shrnb_1_u64:
**	shrnb	z0\.s, z0\.d, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_1_u64, svuint32_t, svuint64_t,
		    z0_res = svshrnb_n_u64 (z0, 1),
		    z0_res = svshrnb (z0, 1))

/*
** shrnb_2_u64:
**	shrnb	z0\.s, z0\.d, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_2_u64, svuint32_t, svuint64_t,
		    z0_res = svshrnb_n_u64 (z0, 2),
		    z0_res = svshrnb (z0, 2))

/*
** shrnb_32_u64_tied1:
**	shrnb	z0\.s, z0\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_32_u64_tied1, svuint32_t, svuint64_t,
		    z0_res = svshrnb_n_u64 (z0, 32),
		    z0_res = svshrnb (z0, 32))

/*
** shrnb_32_u64_untied:
**	shrnb	z0\.s, z1\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_32_u64_untied, svuint32_t, svuint64_t,
		    z0_res = svshrnb_n_u64 (z1, 32),
		    z0_res = svshrnb (z1, 32))
