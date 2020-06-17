/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshrnb_1_u32:
**	uqshrnb	z0\.h, z0\.s, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_1_u32, svuint16_t, svuint32_t,
		    z0_res = svqshrnb_n_u32 (z0, 1),
		    z0_res = svqshrnb (z0, 1))

/*
** qshrnb_2_u32:
**	uqshrnb	z0\.h, z0\.s, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_2_u32, svuint16_t, svuint32_t,
		    z0_res = svqshrnb_n_u32 (z0, 2),
		    z0_res = svqshrnb (z0, 2))

/*
** qshrnb_16_u32_tied1:
**	uqshrnb	z0\.h, z0\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_16_u32_tied1, svuint16_t, svuint32_t,
		    z0_res = svqshrnb_n_u32 (z0, 16),
		    z0_res = svqshrnb (z0, 16))

/*
** qshrnb_16_u32_untied:
**	uqshrnb	z0\.h, z1\.s, #16
**	ret
*/
TEST_TYPE_CHANGE_Z (qshrnb_16_u32_untied, svuint16_t, svuint32_t,
		    z0_res = svqshrnb_n_u32 (z1, 16),
		    z0_res = svqshrnb (z1, 16))
