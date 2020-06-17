/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shllb_0_u16_tied1:
**	ushllb	z0\.h, z0\.b, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_0_u16_tied1, svuint16_t, svuint8_t,
		    z0_res = svshllb_n_u16 (z0, 0),
		    z0_res = svshllb (z0, 0))

/*
** shllb_0_u16_untied:
**	ushllb	z0\.h, z1\.b, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_0_u16_untied, svuint16_t, svuint8_t,
		    z0_res = svshllb_n_u16 (z1, 0),
		    z0_res = svshllb (z1, 0))

/*
** shllb_1_u16_tied1:
**	ushllb	z0\.h, z0\.b, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_1_u16_tied1, svuint16_t, svuint8_t,
		    z0_res = svshllb_n_u16 (z0, 1),
		    z0_res = svshllb (z0, 1))

/*
** shllb_1_u16_untied:
**	ushllb	z0\.h, z1\.b, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_1_u16_untied, svuint16_t, svuint8_t,
		    z0_res = svshllb_n_u16 (z1, 1),
		    z0_res = svshllb (z1, 1))

/*
** shllb_7_u16_tied1:
**	ushllb	z0\.h, z0\.b, #7
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_7_u16_tied1, svuint16_t, svuint8_t,
		    z0_res = svshllb_n_u16 (z0, 7),
		    z0_res = svshllb (z0, 7))

/*
** shllb_7_u16_untied:
**	ushllb	z0\.h, z1\.b, #7
**	ret
*/
TEST_TYPE_CHANGE_Z (shllb_7_u16_untied, svuint16_t, svuint8_t,
		    z0_res = svshllb_n_u16 (z1, 7),
		    z0_res = svshllb (z1, 7))
