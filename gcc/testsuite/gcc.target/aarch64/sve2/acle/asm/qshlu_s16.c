/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshlu_0_s16_m_tied1:
**	sqshlu	z0\.h, p0/m, z0\.h, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s16_m_tied1, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_m (p0, z0, 0),
		 z0_res = svqshlu_m (p0, z0, 0))

/*
** qshlu_0_s16_m_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.h, p0/m, z0\.h, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s16_m_untied, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_m (p0, z1, 0),
		 z0_res = svqshlu_m (p0, z1, 0))

/*
** qshlu_1_s16_m_tied1:
**	sqshlu	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s16_m_tied1, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_m (p0, z0, 1),
		 z0_res = svqshlu_m (p0, z0, 1))

/*
** qshlu_1_s16_m_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s16_m_untied, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_m (p0, z1, 1),
		 z0_res = svqshlu_m (p0, z1, 1))

/*
** qshlu_15_s16_m_tied1:
**	sqshlu	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_DUAL_Z_REV (qshlu_15_s16_m_tied1, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_m (p0, z0, 15),
		 z0_res = svqshlu_m (p0, z0, 15))

/*
** qshlu_15_s16_m_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_DUAL_Z_REV (qshlu_15_s16_m_untied, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_m (p0, z1, 15),
		 z0_res = svqshlu_m (p0, z1, 15))

/*
** qshlu_0_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqshlu	z0\.h, p0/m, z0\.h, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s16_z_tied1, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_z (p0, z0, 0),
		 z0_res = svqshlu_z (p0, z0, 0))

/*
** qshlu_0_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	sqshlu	z0\.h, p0/m, z0\.h, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s16_z_untied, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_z (p0, z1, 0),
		 z0_res = svqshlu_z (p0, z1, 0))

/*
** qshlu_1_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqshlu	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s16_z_tied1, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_z (p0, z0, 1),
		 z0_res = svqshlu_z (p0, z0, 1))

/*
** qshlu_1_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	sqshlu	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s16_z_untied, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_z (p0, z1, 1),
		 z0_res = svqshlu_z (p0, z1, 1))

/*
** qshlu_15_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqshlu	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_DUAL_Z_REV (qshlu_15_s16_z_tied1, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_z (p0, z0, 15),
		 z0_res = svqshlu_z (p0, z0, 15))

/*
** qshlu_15_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	sqshlu	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_DUAL_Z_REV (qshlu_15_s16_z_untied, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_z (p0, z1, 15),
		 z0_res = svqshlu_z (p0, z1, 15))

/*
** qshlu_0_s16_x_tied1:
**	sqshlu	z0\.h, p0/m, z0\.h, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s16_x_tied1, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_x (p0, z0, 0),
		 z0_res = svqshlu_x (p0, z0, 0))

/*
** qshlu_0_s16_x_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.h, p0/m, z0\.h, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s16_x_untied, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_x (p0, z1, 0),
		 z0_res = svqshlu_x (p0, z1, 0))

/*
** qshlu_1_s16_x_tied1:
**	sqshlu	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s16_x_tied1, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_x (p0, z0, 1),
		 z0_res = svqshlu_x (p0, z0, 1))

/*
** qshlu_1_s16_x_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s16_x_untied, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_x (p0, z1, 1),
		 z0_res = svqshlu_x (p0, z1, 1))

/*
** qshlu_15_s16_x_tied1:
**	sqshlu	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_DUAL_Z_REV (qshlu_15_s16_x_tied1, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_x (p0, z0, 15),
		 z0_res = svqshlu_x (p0, z0, 15))

/*
** qshlu_15_s16_x_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_DUAL_Z_REV (qshlu_15_s16_x_untied, svuint16_t, svint16_t,
		 z0_res = svqshlu_n_s16_x (p0, z1, 15),
		 z0_res = svqshlu_x (p0, z1, 15))
