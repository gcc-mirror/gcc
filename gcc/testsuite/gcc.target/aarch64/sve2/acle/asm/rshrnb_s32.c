/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshrnb_1_s32:
**	rshrnb	z0\.h, z0\.s, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (rshrnb_1_s32, svint16_t, svint32_t,
		    z0_res = svrshrnb_n_s32 (z0, 1),
		    z0_res = svrshrnb (z0, 1))

/*
** rshrnb_2_s32:
**	rshrnb	z0\.h, z0\.s, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (rshrnb_2_s32, svint16_t, svint32_t,
		    z0_res = svrshrnb_n_s32 (z0, 2),
		    z0_res = svrshrnb (z0, 2))

/*
** rshrnb_16_s32_tied1:
**	rshrnb	z0\.h, z0\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (rshrnb_16_s32_tied1, svint16_t, svint32_t,
		    z0_res = svrshrnb_n_s32 (z0, 16),
		    z0_res = svrshrnb (z0, 16))

/*
** rshrnb_16_s32_untied:
**	rshrnb	z0\.h, z1\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (rshrnb_16_s32_untied, svint16_t, svint32_t,
		    z0_res = svrshrnb_n_s32 (z1, 16),
		    z0_res = svrshrnb (z1, 16))
