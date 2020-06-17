/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshr_1_s8_m_tied1:
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s8_m_tied1, svint8_t,
		z0 = svrshr_n_s8_m (p0, z0, 1),
		z0 = svrshr_m (p0, z0, 1))

/*
** rshr_1_s8_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s8_m_untied, svint8_t,
		z0 = svrshr_n_s8_m (p0, z1, 1),
		z0 = svrshr_m (p0, z1, 1))

/*
** rshr_2_s8_m_tied1:
**	srshr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s8_m_tied1, svint8_t,
		z0 = svrshr_n_s8_m (p0, z0, 2),
		z0 = svrshr_m (p0, z0, 2))

/*
** rshr_2_s8_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s8_m_untied, svint8_t,
		z0 = svrshr_n_s8_m (p0, z1, 2),
		z0 = svrshr_m (p0, z1, 2))

/*
** rshr_8_s8_m_tied1:
**	srshr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (rshr_8_s8_m_tied1, svint8_t,
		z0 = svrshr_n_s8_m (p0, z0, 8),
		z0 = svrshr_m (p0, z0, 8))

/*
** rshr_8_s8_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (rshr_8_s8_m_untied, svint8_t,
		z0 = svrshr_n_s8_m (p0, z1, 8),
		z0 = svrshr_m (p0, z1, 8))

/*
** rshr_1_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s8_z_tied1, svint8_t,
		z0 = svrshr_n_s8_z (p0, z0, 1),
		z0 = svrshr_z (p0, z0, 1))

/*
** rshr_1_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s8_z_untied, svint8_t,
		z0 = svrshr_n_s8_z (p0, z1, 1),
		z0 = svrshr_z (p0, z1, 1))

/*
** rshr_2_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	srshr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s8_z_tied1, svint8_t,
		z0 = svrshr_n_s8_z (p0, z0, 2),
		z0 = svrshr_z (p0, z0, 2))

/*
** rshr_2_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	srshr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s8_z_untied, svint8_t,
		z0 = svrshr_n_s8_z (p0, z1, 2),
		z0 = svrshr_z (p0, z1, 2))

/*
** rshr_8_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	srshr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (rshr_8_s8_z_tied1, svint8_t,
		z0 = svrshr_n_s8_z (p0, z0, 8),
		z0 = svrshr_z (p0, z0, 8))

/*
** rshr_8_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	srshr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (rshr_8_s8_z_untied, svint8_t,
		z0 = svrshr_n_s8_z (p0, z1, 8),
		z0 = svrshr_z (p0, z1, 8))

/*
** rshr_1_s8_x_tied1:
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s8_x_tied1, svint8_t,
		z0 = svrshr_n_s8_x (p0, z0, 1),
		z0 = svrshr_x (p0, z0, 1))

/*
** rshr_1_s8_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s8_x_untied, svint8_t,
		z0 = svrshr_n_s8_x (p0, z1, 1),
		z0 = svrshr_x (p0, z1, 1))

/*
** rshr_2_s8_x_tied1:
**	srshr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s8_x_tied1, svint8_t,
		z0 = svrshr_n_s8_x (p0, z0, 2),
		z0 = svrshr_x (p0, z0, 2))

/*
** rshr_2_s8_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s8_x_untied, svint8_t,
		z0 = svrshr_n_s8_x (p0, z1, 2),
		z0 = svrshr_x (p0, z1, 2))

/*
** rshr_8_s8_x_tied1:
**	srshr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (rshr_8_s8_x_tied1, svint8_t,
		z0 = svrshr_n_s8_x (p0, z0, 8),
		z0 = svrshr_x (p0, z0, 8))

/*
** rshr_8_s8_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (rshr_8_s8_x_untied, svint8_t,
		z0 = svrshr_n_s8_x (p0, z1, 8),
		z0 = svrshr_x (p0, z1, 8))
