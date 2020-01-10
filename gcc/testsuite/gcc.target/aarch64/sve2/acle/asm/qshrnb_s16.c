/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrnb_1_s16:
**	sqshrnb	z0\.b, z0\.h, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_1_s16, svint8_t, svint16_t,
		    z0_res = svqshrnb_n_s16 (z0, 1),
		    z0_res = svqshrnb (z0, 1))

/*
** qshrnb_2_s16:
**	sqshrnb	z0\.b, z0\.h, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_2_s16, svint8_t, svint16_t,
		    z0_res = svqshrnb_n_s16 (z0, 2),
		    z0_res = svqshrnb (z0, 2))

/*
** qshrnb_8_s16_tied1:
**	sqshrnb	z0\.b, z0\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_8_s16_tied1, svint8_t, svint16_t,
		    z0_res = svqshrnb_n_s16 (z0, 8),
		    z0_res = svqshrnb (z0, 8))

/*
** qshrnb_8_s16_untied:
**	sqshrnb	z0\.b, z1\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_8_s16_untied, svint8_t, svint16_t,
		    z0_res = svqshrnb_n_s16 (z1, 8),
		    z0_res = svqshrnb (z1, 8))
