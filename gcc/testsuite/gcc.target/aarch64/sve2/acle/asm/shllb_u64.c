/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shllb_0_u64_tied1:
**	ushllb	z0\.d, z0\.s, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_0_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svshllb_n_u64 (z0, 0),
		    z0_res = svshllb (z0, 0))

/*
** shllb_0_u64_untied:
**	ushllb	z0\.d, z1\.s, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_0_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svshllb_n_u64 (z1, 0),
		    z0_res = svshllb (z1, 0))

/*
** shllb_1_u64_tied1:
**	ushllb	z0\.d, z0\.s, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_1_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svshllb_n_u64 (z0, 1),
		    z0_res = svshllb (z0, 1))

/*
** shllb_1_u64_untied:
**	ushllb	z0\.d, z1\.s, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_1_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svshllb_n_u64 (z1, 1),
		    z0_res = svshllb (z1, 1))

/*
** shllb_31_u64_tied1:
**	ushllb	z0\.d, z0\.s, #31
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_31_u64_tied1, svuint64_t, svuint32_t,
		    z0_res = svshllb_n_u64 (z0, 31),
		    z0_res = svshllb (z0, 31))

/*
** shllb_31_u64_untied:
**	ushllb	z0\.d, z1\.s, #31
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_31_u64_untied, svuint64_t, svuint32_t,
		    z0_res = svshllb_n_u64 (z1, 31),
		    z0_res = svshllb (z1, 31))
