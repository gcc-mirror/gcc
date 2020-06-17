/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrnb_1_u32:
**	uqrshrnb	z0\.h, z0\.s, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_1_u32, svuint16_t, svuint32_t,
		    z0_res = svqrshrnb_n_u32 (z0, 1),
		    z0_res = svqrshrnb (z0, 1))

/*
** qrshrnb_2_u32:
**	uqrshrnb	z0\.h, z0\.s, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_2_u32, svuint16_t, svuint32_t,
		    z0_res = svqrshrnb_n_u32 (z0, 2),
		    z0_res = svqrshrnb (z0, 2))

/*
** qrshrnb_16_u32_tied1:
**	uqrshrnb	z0\.h, z0\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_16_u32_tied1, svuint16_t, svuint32_t,
		    z0_res = svqrshrnb_n_u32 (z0, 16),
		    z0_res = svqrshrnb (z0, 16))

/*
** qrshrnb_16_u32_untied:
**	uqrshrnb	z0\.h, z1\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_16_u32_untied, svuint16_t, svuint32_t,
		    z0_res = svqrshrnb_n_u32 (z1, 16),
		    z0_res = svqrshrnb (z1, 16))
