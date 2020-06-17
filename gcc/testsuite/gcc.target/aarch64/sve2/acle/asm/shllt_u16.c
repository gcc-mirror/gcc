/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** shllt_0_u16_tied1:
**	ushllt	z0\.h, z0\.b, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_0_u16_tied1, svuint16_t, svuint8_t,
		    z0_res = svshllt_n_u16 (z0, 0),
		    z0_res = svshllt (z0, 0))

/*
** shllt_0_u16_untied:
**	ushllt	z0\.h, z1\.b, #0
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_0_u16_untied, svuint16_t, svuint8_t,
		    z0_res = svshllt_n_u16 (z1, 0),
		    z0_res = svshllt (z1, 0))

/*
** shllt_1_u16_tied1:
**	ushllt	z0\.h, z0\.b, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_1_u16_tied1, svuint16_t, svuint8_t,
		    z0_res = svshllt_n_u16 (z0, 1),
		    z0_res = svshllt (z0, 1))

/*
** shllt_1_u16_untied:
**	ushllt	z0\.h, z1\.b, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_1_u16_untied, svuint16_t, svuint8_t,
		    z0_res = svshllt_n_u16 (z1, 1),
		    z0_res = svshllt (z1, 1))

/*
** shllt_7_u16_tied1:
**	ushllt	z0\.h, z0\.b, #7
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_7_u16_tied1, svuint16_t, svuint8_t,
		    z0_res = svshllt_n_u16 (z0, 7),
		    z0_res = svshllt (z0, 7))

/*
** shllt_7_u16_untied:
**	ushllt	z0\.h, z1\.b, #7
**	ret
*/
TEST_TYPE_CHANGE_Z (shllt_7_u16_untied, svuint16_t, svuint8_t,
		    z0_res = svshllt_n_u16 (z1, 7),
		    z0_res = svshllt (z1, 7))
