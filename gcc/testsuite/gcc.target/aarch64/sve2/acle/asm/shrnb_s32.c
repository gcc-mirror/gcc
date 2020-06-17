/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shrnb_1_s32:
**	shrnb	z0\.h, z0\.s, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_1_s32, svint16_t, svint32_t,
		    z0_res = svshrnb_n_s32 (z0, 1),
		    z0_res = svshrnb (z0, 1))

/*
** shrnb_2_s32:
**	shrnb	z0\.h, z0\.s, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_2_s32, svint16_t, svint32_t,
		    z0_res = svshrnb_n_s32 (z0, 2),
		    z0_res = svshrnb (z0, 2))

/*
** shrnb_16_s32_tied1:
**	shrnb	z0\.h, z0\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_16_s32_tied1, svint16_t, svint32_t,
		    z0_res = svshrnb_n_s32 (z0, 16),
		    z0_res = svshrnb (z0, 16))

/*
** shrnb_16_s32_untied:
**	shrnb	z0\.h, z1\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_16_s32_untied, svint16_t, svint32_t,
		    z0_res = svshrnb_n_s32 (z1, 16),
		    z0_res = svshrnb (z1, 16))
