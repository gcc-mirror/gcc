/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shrnb_1_u16:
**	shrnb	z0\.b, z0\.h, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_1_u16, svuint8_t, svuint16_t,
		    z0_res = svshrnb_n_u16 (z0, 1),
		    z0_res = svshrnb (z0, 1))

/*
** shrnb_2_u16:
**	shrnb	z0\.b, z0\.h, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_2_u16, svuint8_t, svuint16_t,
		    z0_res = svshrnb_n_u16 (z0, 2),
		    z0_res = svshrnb (z0, 2))

/*
** shrnb_8_u16_tied1:
**	shrnb	z0\.b, z0\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_8_u16_tied1, svuint8_t, svuint16_t,
		    z0_res = svshrnb_n_u16 (z0, 8),
		    z0_res = svshrnb (z0, 8))

/*
** shrnb_8_u16_untied:
**	shrnb	z0\.b, z1\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_8_u16_untied, svuint8_t, svuint16_t,
		    z0_res = svshrnb_n_u16 (z1, 8),
		    z0_res = svshrnb (z1, 8))
