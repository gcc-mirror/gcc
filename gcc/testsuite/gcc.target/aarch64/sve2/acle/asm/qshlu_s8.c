/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshlu_0_s8_m_tied1:
**	sqshlu	z0\.b, p0/m, z0\.b, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s8_m_tied1, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_m (p0, z0, 0),
		 z0_res = svqshlu_m (p0, z0, 0))

/*
** qshlu_0_s8_m_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.b, p0/m, z0\.b, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s8_m_untied, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_m (p0, z1, 0),
		 z0_res = svqshlu_m (p0, z1, 0))

/*
** qshlu_1_s8_m_tied1:
**	sqshlu	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s8_m_tied1, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_m (p0, z0, 1),
		 z0_res = svqshlu_m (p0, z0, 1))

/*
** qshlu_1_s8_m_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s8_m_untied, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_m (p0, z1, 1),
		 z0_res = svqshlu_m (p0, z1, 1))

/*
** qshlu_7_s8_m_tied1:
**	sqshlu	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_DUAL_Z_REV (qshlu_7_s8_m_tied1, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_m (p0, z0, 7),
		 z0_res = svqshlu_m (p0, z0, 7))

/*
** qshlu_7_s8_m_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_DUAL_Z_REV (qshlu_7_s8_m_untied, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_m (p0, z1, 7),
		 z0_res = svqshlu_m (p0, z1, 7))

/*
** qshlu_0_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	sqshlu	z0\.b, p0/m, z0\.b, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s8_z_tied1, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_z (p0, z0, 0),
		 z0_res = svqshlu_z (p0, z0, 0))

/*
** qshlu_0_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	sqshlu	z0\.b, p0/m, z0\.b, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s8_z_untied, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_z (p0, z1, 0),
		 z0_res = svqshlu_z (p0, z1, 0))

/*
** qshlu_1_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	sqshlu	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s8_z_tied1, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_z (p0, z0, 1),
		 z0_res = svqshlu_z (p0, z0, 1))

/*
** qshlu_1_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	sqshlu	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s8_z_untied, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_z (p0, z1, 1),
		 z0_res = svqshlu_z (p0, z1, 1))

/*
** qshlu_7_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	sqshlu	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_DUAL_Z_REV (qshlu_7_s8_z_tied1, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_z (p0, z0, 7),
		 z0_res = svqshlu_z (p0, z0, 7))

/*
** qshlu_7_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	sqshlu	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_DUAL_Z_REV (qshlu_7_s8_z_untied, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_z (p0, z1, 7),
		 z0_res = svqshlu_z (p0, z1, 7))

/*
** qshlu_0_s8_x_tied1:
**	sqshlu	z0\.b, p0/m, z0\.b, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s8_x_tied1, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_x (p0, z0, 0),
		 z0_res = svqshlu_x (p0, z0, 0))

/*
** qshlu_0_s8_x_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.b, p0/m, z0\.b, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s8_x_untied, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_x (p0, z1, 0),
		 z0_res = svqshlu_x (p0, z1, 0))

/*
** qshlu_1_s8_x_tied1:
**	sqshlu	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s8_x_tied1, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_x (p0, z0, 1),
		 z0_res = svqshlu_x (p0, z0, 1))

/*
** qshlu_1_s8_x_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s8_x_untied, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_x (p0, z1, 1),
		 z0_res = svqshlu_x (p0, z1, 1))

/*
** qshlu_7_s8_x_tied1:
**	sqshlu	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_DUAL_Z_REV (qshlu_7_s8_x_tied1, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_x (p0, z0, 7),
		 z0_res = svqshlu_x (p0, z0, 7))

/*
** qshlu_7_s8_x_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_DUAL_Z_REV (qshlu_7_s8_x_untied, svuint8_t, svint8_t,
		 z0_res = svqshlu_n_s8_x (p0, z1, 7),
		 z0_res = svqshlu_x (p0, z1, 7))
