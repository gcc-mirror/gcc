/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshlu_0_s32_m_tied1:
**	sqshlu	z0\.s, p0/m, z0\.s, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s32_m_tied1, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_m (p0, z0, 0),
		 z0_res = svqshlu_m (p0, z0, 0))

/*
** qshlu_0_s32_m_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.s, p0/m, z0\.s, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s32_m_untied, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_m (p0, z1, 0),
		 z0_res = svqshlu_m (p0, z1, 0))

/*
** qshlu_1_s32_m_tied1:
**	sqshlu	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s32_m_tied1, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_m (p0, z0, 1),
		 z0_res = svqshlu_m (p0, z0, 1))

/*
** qshlu_1_s32_m_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s32_m_untied, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_m (p0, z1, 1),
		 z0_res = svqshlu_m (p0, z1, 1))

/*
** qshlu_31_s32_m_tied1:
**	sqshlu	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_DUAL_Z_REV (qshlu_31_s32_m_tied1, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_m (p0, z0, 31),
		 z0_res = svqshlu_m (p0, z0, 31))

/*
** qshlu_31_s32_m_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_DUAL_Z_REV (qshlu_31_s32_m_untied, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_m (p0, z1, 31),
		 z0_res = svqshlu_m (p0, z1, 31))

/*
** qshlu_0_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	sqshlu	z0\.s, p0/m, z0\.s, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s32_z_tied1, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_z (p0, z0, 0),
		 z0_res = svqshlu_z (p0, z0, 0))

/*
** qshlu_0_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	sqshlu	z0\.s, p0/m, z0\.s, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s32_z_untied, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_z (p0, z1, 0),
		 z0_res = svqshlu_z (p0, z1, 0))

/*
** qshlu_1_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	sqshlu	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s32_z_tied1, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_z (p0, z0, 1),
		 z0_res = svqshlu_z (p0, z0, 1))

/*
** qshlu_1_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	sqshlu	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s32_z_untied, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_z (p0, z1, 1),
		 z0_res = svqshlu_z (p0, z1, 1))

/*
** qshlu_31_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	sqshlu	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_DUAL_Z_REV (qshlu_31_s32_z_tied1, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_z (p0, z0, 31),
		 z0_res = svqshlu_z (p0, z0, 31))

/*
** qshlu_31_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	sqshlu	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_DUAL_Z_REV (qshlu_31_s32_z_untied, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_z (p0, z1, 31),
		 z0_res = svqshlu_z (p0, z1, 31))

/*
** qshlu_0_s32_x_tied1:
**	sqshlu	z0\.s, p0/m, z0\.s, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s32_x_tied1, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_x (p0, z0, 0),
		 z0_res = svqshlu_x (p0, z0, 0))

/*
** qshlu_0_s32_x_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.s, p0/m, z0\.s, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s32_x_untied, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_x (p0, z1, 0),
		 z0_res = svqshlu_x (p0, z1, 0))

/*
** qshlu_1_s32_x_tied1:
**	sqshlu	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s32_x_tied1, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_x (p0, z0, 1),
		 z0_res = svqshlu_x (p0, z0, 1))

/*
** qshlu_1_s32_x_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s32_x_untied, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_x (p0, z1, 1),
		 z0_res = svqshlu_x (p0, z1, 1))

/*
** qshlu_31_s32_x_tied1:
**	sqshlu	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_DUAL_Z_REV (qshlu_31_s32_x_tied1, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_x (p0, z0, 31),
		 z0_res = svqshlu_x (p0, z0, 31))

/*
** qshlu_31_s32_x_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_DUAL_Z_REV (qshlu_31_s32_x_untied, svuint32_t, svint32_t,
		 z0_res = svqshlu_n_s32_x (p0, z1, 31),
		 z0_res = svqshlu_x (p0, z1, 31))
