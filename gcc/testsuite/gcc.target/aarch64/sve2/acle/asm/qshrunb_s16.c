/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrunb_1_s16:
**	sqshrunb	z0\.b, z0\.h, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrunb_1_s16, svuint8_t, svint16_t,
		    z0_res = svqshrunb_n_s16 (z0, 1),
		    z0_res = svqshrunb (z0, 1))

/*
** qshrunb_2_s16:
**	sqshrunb	z0\.b, z0\.h, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrunb_2_s16, svuint8_t, svint16_t,
		    z0_res = svqshrunb_n_s16 (z0, 2),
		    z0_res = svqshrunb (z0, 2))

/*
** qshrunb_8_s16_tied1:
**	sqshrunb	z0\.b, z0\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrunb_8_s16_tied1, svuint8_t, svint16_t,
		    z0_res = svqshrunb_n_s16 (z0, 8),
		    z0_res = svqshrunb (z0, 8))

/*
** qshrunb_8_s16_untied:
**	sqshrunb	z0\.b, z1\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrunb_8_s16_untied, svuint8_t, svint16_t,
		    z0_res = svqshrunb_n_s16 (z1, 8),
		    z0_res = svqshrunb (z1, 8))
