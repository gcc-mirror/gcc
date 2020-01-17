/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshrnb_1_u16:
**	rshrnb	z0\.b, z0\.h, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (rshrnb_1_u16, svuint8_t, svuint16_t,
		    z0_res = svrshrnb_n_u16 (z0, 1),
		    z0_res = svrshrnb (z0, 1))

/*
** rshrnb_2_u16:
**	rshrnb	z0\.b, z0\.h, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (rshrnb_2_u16, svuint8_t, svuint16_t,
		    z0_res = svrshrnb_n_u16 (z0, 2),
		    z0_res = svrshrnb (z0, 2))

/*
** rshrnb_8_u16_tied1:
**	rshrnb	z0\.b, z0\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (rshrnb_8_u16_tied1, svuint8_t, svuint16_t,
		    z0_res = svrshrnb_n_u16 (z0, 8),
		    z0_res = svrshrnb (z0, 8))

/*
** rshrnb_8_u16_untied:
**	rshrnb	z0\.b, z1\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (rshrnb_8_u16_untied, svuint8_t, svuint16_t,
		    z0_res = svrshrnb_n_u16 (z1, 8),
		    z0_res = svrshrnb (z1, 8))
