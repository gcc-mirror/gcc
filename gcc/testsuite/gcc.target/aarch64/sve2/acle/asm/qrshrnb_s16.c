/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qrshrnb_1_s16:
**	sqrshrnb	z0\.b, z0\.h, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_1_s16, svint8_t, svint16_t,
		    z0_res = svqrshrnb_n_s16 (z0, 1),
		    z0_res = svqrshrnb (z0, 1))

/*
** qrshrnb_2_s16:
**	sqrshrnb	z0\.b, z0\.h, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_2_s16, svint8_t, svint16_t,
		    z0_res = svqrshrnb_n_s16 (z0, 2),
		    z0_res = svqrshrnb (z0, 2))

/*
** qrshrnb_8_s16_tied1:
**	sqrshrnb	z0\.b, z0\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_8_s16_tied1, svint8_t, svint16_t,
		    z0_res = svqrshrnb_n_s16 (z0, 8),
		    z0_res = svqrshrnb (z0, 8))

/*
** qrshrnb_8_s16_untied:
**	sqrshrnb	z0\.b, z1\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (qrshrnb_8_s16_untied, svint8_t, svint16_t,
		    z0_res = svqrshrnb_n_s16 (z1, 8),
		    z0_res = svqrshrnb (z1, 8))
