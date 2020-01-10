/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrunb_1_s16:
**	sqrshrunb	z0\.b, z0\.h, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrunb_1_s16, svuint8_t, svint16_t,
		    z0_res = svqrshrunb_n_s16 (z0, 1),
		    z0_res = svqrshrunb (z0, 1))

/*
** qrshrunb_2_s16:
**	sqrshrunb	z0\.b, z0\.h, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrunb_2_s16, svuint8_t, svint16_t,
		    z0_res = svqrshrunb_n_s16 (z0, 2),
		    z0_res = svqrshrunb (z0, 2))

/*
** qrshrunb_8_s16_tied1:
**	sqrshrunb	z0\.b, z0\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrunb_8_s16_tied1, svuint8_t, svint16_t,
		    z0_res = svqrshrunb_n_s16 (z0, 8),
		    z0_res = svqrshrunb (z0, 8))

/*
** qrshrunb_8_s16_untied:
**	sqrshrunb	z0\.b, z1\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrunb_8_s16_untied, svuint8_t, svint16_t,
		    z0_res = svqrshrunb_n_s16 (z1, 8),
		    z0_res = svqrshrunb (z1, 8))
