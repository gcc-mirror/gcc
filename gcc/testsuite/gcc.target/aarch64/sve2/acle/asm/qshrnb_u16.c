/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrnb_1_u16:
**	uqshrnb	z0\.b, z0\.h, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_1_u16, svuint8_t, svuint16_t,
		    z0_res = svqshrnb_n_u16 (z0, 1),
		    z0_res = svqshrnb (z0, 1))

/*
** qshrnb_2_u16:
**	uqshrnb	z0\.b, z0\.h, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_2_u16, svuint8_t, svuint16_t,
		    z0_res = svqshrnb_n_u16 (z0, 2),
		    z0_res = svqshrnb (z0, 2))

/*
** qshrnb_8_u16_tied1:
**	uqshrnb	z0\.b, z0\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_8_u16_tied1, svuint8_t, svuint16_t,
		    z0_res = svqshrnb_n_u16 (z0, 8),
		    z0_res = svqshrnb (z0, 8))

/*
** qshrnb_8_u16_untied:
**	uqshrnb	z0\.b, z1\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_8_u16_untied, svuint8_t, svuint16_t,
		    z0_res = svqshrnb_n_u16 (z1, 8),
		    z0_res = svqshrnb (z1, 8))
