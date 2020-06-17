/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shrnb_1_u32:
**	shrnb	z0\.h, z0\.s, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_1_u32, svuint16_t, svuint32_t,
		    z0_res = svshrnb_n_u32 (z0, 1),
		    z0_res = svshrnb (z0, 1))

/*
** shrnb_2_u32:
**	shrnb	z0\.h, z0\.s, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_2_u32, svuint16_t, svuint32_t,
		    z0_res = svshrnb_n_u32 (z0, 2),
		    z0_res = svshrnb (z0, 2))

/*
** shrnb_16_u32_tied1:
**	shrnb	z0\.h, z0\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_16_u32_tied1, svuint16_t, svuint32_t,
		    z0_res = svshrnb_n_u32 (z0, 16),
		    z0_res = svshrnb (z0, 16))

/*
** shrnb_16_u32_untied:
**	shrnb	z0\.h, z1\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_16_u32_untied, svuint16_t, svuint32_t,
		    z0_res = svshrnb_n_u32 (z1, 16),
		    z0_res = svshrnb (z1, 16))
