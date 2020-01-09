/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrunb_1_s32:
**	sqrshrunb	z0\.h, z0\.s, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrunb_1_s32, svuint16_t, svint32_t,
		    z0_res = svqrshrunb_n_s32 (z0, 1),
		    z0_res = svqrshrunb (z0, 1))

/*
** qrshrunb_2_s32:
**	sqrshrunb	z0\.h, z0\.s, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrunb_2_s32, svuint16_t, svint32_t,
		    z0_res = svqrshrunb_n_s32 (z0, 2),
		    z0_res = svqrshrunb (z0, 2))

/*
** qrshrunb_16_s32_tied1:
**	sqrshrunb	z0\.h, z0\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrunb_16_s32_tied1, svuint16_t, svint32_t,
		    z0_res = svqrshrunb_n_s32 (z0, 16),
		    z0_res = svqrshrunb (z0, 16))

/*
** qrshrunb_16_s32_untied:
**	sqrshrunb	z0\.h, z1\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrunb_16_s32_untied, svuint16_t, svint32_t,
		    z0_res = svqrshrunb_n_s32 (z1, 16),
		    z0_res = svqrshrunb (z1, 16))
