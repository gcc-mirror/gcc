/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrunb_1_s32:
**	sqshrunb	z0\.h, z0\.s, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrunb_1_s32, svuint16_t, svint32_t,
		    z0_res = svqshrunb_n_s32 (z0, 1),
		    z0_res = svqshrunb (z0, 1))

/*
** qshrunb_2_s32:
**	sqshrunb	z0\.h, z0\.s, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrunb_2_s32, svuint16_t, svint32_t,
		    z0_res = svqshrunb_n_s32 (z0, 2),
		    z0_res = svqshrunb (z0, 2))

/*
** qshrunb_16_s32_tied1:
**	sqshrunb	z0\.h, z0\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrunb_16_s32_tied1, svuint16_t, svint32_t,
		    z0_res = svqshrunb_n_s32 (z0, 16),
		    z0_res = svqshrunb (z0, 16))

/*
** qshrunb_16_s32_untied:
**	sqshrunb	z0\.h, z1\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrunb_16_s32_untied, svuint16_t, svint32_t,
		    z0_res = svqshrunb_n_s32 (z1, 16),
		    z0_res = svqshrunb (z1, 16))
