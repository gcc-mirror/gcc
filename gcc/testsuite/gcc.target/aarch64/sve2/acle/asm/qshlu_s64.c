/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qshlu_0_s64_m_tied1:
**	sqshlu	z0\.d, p0/m, z0\.d, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s64_m_tied1, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_m (p0, z0, 0),
		 z0_res = svqshlu_m (p0, z0, 0))

/*
** qshlu_0_s64_m_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.d, p0/m, z0\.d, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s64_m_untied, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_m (p0, z1, 0),
		 z0_res = svqshlu_m (p0, z1, 0))

/*
** qshlu_1_s64_m_tied1:
**	sqshlu	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s64_m_tied1, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_m (p0, z0, 1),
		 z0_res = svqshlu_m (p0, z0, 1))

/*
** qshlu_1_s64_m_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s64_m_untied, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_m (p0, z1, 1),
		 z0_res = svqshlu_m (p0, z1, 1))

/*
** qshlu_63_s64_m_tied1:
**	sqshlu	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_DUAL_Z_REV (qshlu_63_s64_m_tied1, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_m (p0, z0, 63),
		 z0_res = svqshlu_m (p0, z0, 63))

/*
** qshlu_63_s64_m_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_DUAL_Z_REV (qshlu_63_s64_m_untied, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_m (p0, z1, 63),
		 z0_res = svqshlu_m (p0, z1, 63))

/*
** qshlu_0_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	sqshlu	z0\.d, p0/m, z0\.d, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s64_z_tied1, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_z (p0, z0, 0),
		 z0_res = svqshlu_z (p0, z0, 0))

/*
** qshlu_0_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	sqshlu	z0\.d, p0/m, z0\.d, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s64_z_untied, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_z (p0, z1, 0),
		 z0_res = svqshlu_z (p0, z1, 0))

/*
** qshlu_1_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	sqshlu	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s64_z_tied1, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_z (p0, z0, 1),
		 z0_res = svqshlu_z (p0, z0, 1))

/*
** qshlu_1_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	sqshlu	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s64_z_untied, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_z (p0, z1, 1),
		 z0_res = svqshlu_z (p0, z1, 1))

/*
** qshlu_63_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	sqshlu	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_DUAL_Z_REV (qshlu_63_s64_z_tied1, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_z (p0, z0, 63),
		 z0_res = svqshlu_z (p0, z0, 63))

/*
** qshlu_63_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	sqshlu	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_DUAL_Z_REV (qshlu_63_s64_z_untied, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_z (p0, z1, 63),
		 z0_res = svqshlu_z (p0, z1, 63))

/*
** qshlu_0_s64_x_tied1:
**	sqshlu	z0\.d, p0/m, z0\.d, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s64_x_tied1, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_x (p0, z0, 0),
		 z0_res = svqshlu_x (p0, z0, 0))

/*
** qshlu_0_s64_x_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.d, p0/m, z0\.d, #0
**	ret
*/
TEST_DUAL_Z_REV (qshlu_0_s64_x_untied, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_x (p0, z1, 0),
		 z0_res = svqshlu_x (p0, z1, 0))

/*
** qshlu_1_s64_x_tied1:
**	sqshlu	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s64_x_tied1, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_x (p0, z0, 1),
		 z0_res = svqshlu_x (p0, z0, 1))

/*
** qshlu_1_s64_x_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_DUAL_Z_REV (qshlu_1_s64_x_untied, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_x (p0, z1, 1),
		 z0_res = svqshlu_x (p0, z1, 1))

/*
** qshlu_63_s64_x_tied1:
**	sqshlu	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_DUAL_Z_REV (qshlu_63_s64_x_tied1, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_x (p0, z0, 63),
		 z0_res = svqshlu_x (p0, z0, 63))

/*
** qshlu_63_s64_x_untied:
**	movprfx	z0, z1
**	sqshlu	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_DUAL_Z_REV (qshlu_63_s64_x_untied, svuint64_t, svint64_t,
		 z0_res = svqshlu_n_s64_x (p0, z1, 63),
		 z0_res = svqshlu_x (p0, z1, 63))
