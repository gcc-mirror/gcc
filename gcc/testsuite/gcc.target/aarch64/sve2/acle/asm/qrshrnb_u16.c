/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrnb_1_u16:
**	uqrshrnb	z0\.b, z0\.h, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_1_u16, svuint8_t, svuint16_t,
		    z0_res = svqrshrnb_n_u16 (z0, 1),
		    z0_res = svqrshrnb (z0, 1))

/*
** qrshrnb_2_u16:
**	uqrshrnb	z0\.b, z0\.h, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_2_u16, svuint8_t, svuint16_t,
		    z0_res = svqrshrnb_n_u16 (z0, 2),
		    z0_res = svqrshrnb (z0, 2))

/*
** qrshrnb_8_u16_tied1:
**	uqrshrnb	z0\.b, z0\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_8_u16_tied1, svuint8_t, svuint16_t,
		    z0_res = svqrshrnb_n_u16 (z0, 8),
		    z0_res = svqrshrnb (z0, 8))

/*
** qrshrnb_8_u16_untied:
**	uqrshrnb	z0\.b, z1\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_8_u16_untied, svuint8_t, svuint16_t,
		    z0_res = svqrshrnb_n_u16 (z1, 8),
		    z0_res = svqrshrnb (z1, 8))
