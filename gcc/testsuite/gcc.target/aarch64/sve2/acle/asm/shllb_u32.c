/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shllb_0_u32_tied1:
**	ushllb	z0\.s, z0\.h, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_0_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svshllb_n_u32 (z0, 0),
		    z0_res = svshllb (z0, 0))

/*
** shllb_0_u32_untied:
**	ushllb	z0\.s, z1\.h, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_0_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svshllb_n_u32 (z1, 0),
		    z0_res = svshllb (z1, 0))

/*
** shllb_1_u32_tied1:
**	ushllb	z0\.s, z0\.h, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_1_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svshllb_n_u32 (z0, 1),
		    z0_res = svshllb (z0, 1))

/*
** shllb_1_u32_untied:
**	ushllb	z0\.s, z1\.h, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_1_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svshllb_n_u32 (z1, 1),
		    z0_res = svshllb (z1, 1))

/*
** shllb_15_u32_tied1:
**	ushllb	z0\.s, z0\.h, #15
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_15_u32_tied1, svuint32_t, svuint16_t,
		    z0_res = svshllb_n_u32 (z0, 15),
		    z0_res = svshllb (z0, 15))

/*
** shllb_15_u32_untied:
**	ushllb	z0\.s, z1\.h, #15
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_15_u32_untied, svuint32_t, svuint16_t,
		    z0_res = svshllb_n_u32 (z1, 15),
		    z0_res = svshllb (z1, 15))
