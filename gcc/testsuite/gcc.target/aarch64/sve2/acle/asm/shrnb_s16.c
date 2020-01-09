/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shrnb_1_s16:
**	shrnb	z0\.b, z0\.h, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_1_s16, svint8_t, svint16_t,
		    z0_res = svshrnb_n_s16 (z0, 1),
		    z0_res = svshrnb (z0, 1))

/*
** shrnb_2_s16:
**	shrnb	z0\.b, z0\.h, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_2_s16, svint8_t, svint16_t,
		    z0_res = svshrnb_n_s16 (z0, 2),
		    z0_res = svshrnb (z0, 2))

/*
** shrnb_8_s16_tied1:
**	shrnb	z0\.b, z0\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_8_s16_tied1, svint8_t, svint16_t,
		    z0_res = svshrnb_n_s16 (z0, 8),
		    z0_res = svshrnb (z0, 8))

/*
** shrnb_8_s16_untied:
**	shrnb	z0\.b, z1\.h, #8
**	ret
*/
TEST_TYPE_CHANGE_Z (shrnb_8_s16_untied, svint8_t, svint16_t,
		    z0_res = svshrnb_n_s16 (z1, 8),
		    z0_res = svshrnb (z1, 8))
