/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrnb_1_s32:
**	sqrshrnb	z0\.h, z0\.s, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_1_s32, svint16_t, svint32_t,
		    z0_res = svqrshrnb_n_s32 (z0, 1),
		    z0_res = svqrshrnb (z0, 1))

/*
** qrshrnb_2_s32:
**	sqrshrnb	z0\.h, z0\.s, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_2_s32, svint16_t, svint32_t,
		    z0_res = svqrshrnb_n_s32 (z0, 2),
		    z0_res = svqrshrnb (z0, 2))

/*
** qrshrnb_16_s32_tied1:
**	sqrshrnb	z0\.h, z0\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_16_s32_tied1, svint16_t, svint32_t,
		    z0_res = svqrshrnb_n_s32 (z0, 16),
		    z0_res = svqrshrnb (z0, 16))

/*
** qrshrnb_16_s32_untied:
**	sqrshrnb	z0\.h, z1\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_16_s32_untied, svint16_t, svint32_t,
		    z0_res = svqrshrnb_n_s32 (z1, 16),
		    z0_res = svqrshrnb (z1, 16))
