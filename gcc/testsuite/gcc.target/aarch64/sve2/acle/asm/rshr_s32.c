/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshr_1_s32_m_tied1:
**	srshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s32_m_tied1, svint32_t,
		z0 = svrshr_n_s32_m (p0, z0, 1),
		z0 = svrshr_m (p0, z0, 1))

/*
** rshr_1_s32_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s32_m_untied, svint32_t,
		z0 = svrshr_n_s32_m (p0, z1, 1),
		z0 = svrshr_m (p0, z1, 1))

/*
** rshr_2_s32_m_tied1:
**	srshr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s32_m_tied1, svint32_t,
		z0 = svrshr_n_s32_m (p0, z0, 2),
		z0 = svrshr_m (p0, z0, 2))

/*
** rshr_2_s32_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s32_m_untied, svint32_t,
		z0 = svrshr_n_s32_m (p0, z1, 2),
		z0 = svrshr_m (p0, z1, 2))

/*
** rshr_32_s32_m_tied1:
**	srshr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (rshr_32_s32_m_tied1, svint32_t,
		z0 = svrshr_n_s32_m (p0, z0, 32),
		z0 = svrshr_m (p0, z0, 32))

/*
** rshr_32_s32_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (rshr_32_s32_m_untied, svint32_t,
		z0 = svrshr_n_s32_m (p0, z1, 32),
		z0 = svrshr_m (p0, z1, 32))

/*
** rshr_1_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	srshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s32_z_tied1, svint32_t,
		z0 = svrshr_n_s32_z (p0, z0, 1),
		z0 = svrshr_z (p0, z0, 1))

/*
** rshr_1_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	srshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s32_z_untied, svint32_t,
		z0 = svrshr_n_s32_z (p0, z1, 1),
		z0 = svrshr_z (p0, z1, 1))

/*
** rshr_2_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	srshr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s32_z_tied1, svint32_t,
		z0 = svrshr_n_s32_z (p0, z0, 2),
		z0 = svrshr_z (p0, z0, 2))

/*
** rshr_2_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	srshr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s32_z_untied, svint32_t,
		z0 = svrshr_n_s32_z (p0, z1, 2),
		z0 = svrshr_z (p0, z1, 2))

/*
** rshr_32_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	srshr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (rshr_32_s32_z_tied1, svint32_t,
		z0 = svrshr_n_s32_z (p0, z0, 32),
		z0 = svrshr_z (p0, z0, 32))

/*
** rshr_32_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	srshr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (rshr_32_s32_z_untied, svint32_t,
		z0 = svrshr_n_s32_z (p0, z1, 32),
		z0 = svrshr_z (p0, z1, 32))

/*
** rshr_1_s32_x_tied1:
**	srshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s32_x_tied1, svint32_t,
		z0 = svrshr_n_s32_x (p0, z0, 1),
		z0 = svrshr_x (p0, z0, 1))

/*
** rshr_1_s32_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s32_x_untied, svint32_t,
		z0 = svrshr_n_s32_x (p0, z1, 1),
		z0 = svrshr_x (p0, z1, 1))

/*
** rshr_2_s32_x_tied1:
**	srshr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s32_x_tied1, svint32_t,
		z0 = svrshr_n_s32_x (p0, z0, 2),
		z0 = svrshr_x (p0, z0, 2))

/*
** rshr_2_s32_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s32_x_untied, svint32_t,
		z0 = svrshr_n_s32_x (p0, z1, 2),
		z0 = svrshr_x (p0, z1, 2))

/*
** rshr_32_s32_x_tied1:
**	srshr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (rshr_32_s32_x_tied1, svint32_t,
		z0 = svrshr_n_s32_x (p0, z0, 32),
		z0 = svrshr_x (p0, z0, 32))

/*
** rshr_32_s32_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (rshr_32_s32_x_untied, svint32_t,
		z0 = svrshr_n_s32_x (p0, z1, 32),
		z0 = svrshr_x (p0, z1, 32))
