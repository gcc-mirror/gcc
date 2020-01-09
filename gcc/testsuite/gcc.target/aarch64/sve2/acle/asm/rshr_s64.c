/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshr_1_s64_m_tied1:
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s64_m_tied1, svint64_t,
		z0 = svrshr_n_s64_m (p0, z0, 1),
		z0 = svrshr_m (p0, z0, 1))

/*
** rshr_1_s64_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s64_m_untied, svint64_t,
		z0 = svrshr_n_s64_m (p0, z1, 1),
		z0 = svrshr_m (p0, z1, 1))

/*
** rshr_2_s64_m_tied1:
**	srshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s64_m_tied1, svint64_t,
		z0 = svrshr_n_s64_m (p0, z0, 2),
		z0 = svrshr_m (p0, z0, 2))

/*
** rshr_2_s64_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s64_m_untied, svint64_t,
		z0 = svrshr_n_s64_m (p0, z1, 2),
		z0 = svrshr_m (p0, z1, 2))

/*
** rshr_64_s64_m_tied1:
**	srshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rshr_64_s64_m_tied1, svint64_t,
		z0 = svrshr_n_s64_m (p0, z0, 64),
		z0 = svrshr_m (p0, z0, 64))

/*
** rshr_64_s64_m_untied:
**	movprfx	z0, z1
**	srshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rshr_64_s64_m_untied, svint64_t,
		z0 = svrshr_n_s64_m (p0, z1, 64),
		z0 = svrshr_m (p0, z1, 64))

/*
** rshr_1_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s64_z_tied1, svint64_t,
		z0 = svrshr_n_s64_z (p0, z0, 1),
		z0 = svrshr_z (p0, z0, 1))

/*
** rshr_1_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s64_z_untied, svint64_t,
		z0 = svrshr_n_s64_z (p0, z1, 1),
		z0 = svrshr_z (p0, z1, 1))

/*
** rshr_2_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	srshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s64_z_tied1, svint64_t,
		z0 = svrshr_n_s64_z (p0, z0, 2),
		z0 = svrshr_z (p0, z0, 2))

/*
** rshr_2_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	srshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s64_z_untied, svint64_t,
		z0 = svrshr_n_s64_z (p0, z1, 2),
		z0 = svrshr_z (p0, z1, 2))

/*
** rshr_64_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	srshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rshr_64_s64_z_tied1, svint64_t,
		z0 = svrshr_n_s64_z (p0, z0, 64),
		z0 = svrshr_z (p0, z0, 64))

/*
** rshr_64_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	srshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rshr_64_s64_z_untied, svint64_t,
		z0 = svrshr_n_s64_z (p0, z1, 64),
		z0 = svrshr_z (p0, z1, 64))

/*
** rshr_1_s64_x_tied1:
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s64_x_tied1, svint64_t,
		z0 = svrshr_n_s64_x (p0, z0, 1),
		z0 = svrshr_x (p0, z0, 1))

/*
** rshr_1_s64_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_s64_x_untied, svint64_t,
		z0 = svrshr_n_s64_x (p0, z1, 1),
		z0 = svrshr_x (p0, z1, 1))

/*
** rshr_2_s64_x_tied1:
**	srshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s64_x_tied1, svint64_t,
		z0 = svrshr_n_s64_x (p0, z0, 2),
		z0 = svrshr_x (p0, z0, 2))

/*
** rshr_2_s64_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_s64_x_untied, svint64_t,
		z0 = svrshr_n_s64_x (p0, z1, 2),
		z0 = svrshr_x (p0, z1, 2))

/*
** rshr_64_s64_x_tied1:
**	srshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rshr_64_s64_x_tied1, svint64_t,
		z0 = svrshr_n_s64_x (p0, z0, 64),
		z0 = svrshr_x (p0, z0, 64))

/*
** rshr_64_s64_x_untied:
**	movprfx	z0, z1
**	srshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rshr_64_s64_x_untied, svint64_t,
		z0 = svrshr_n_s64_x (p0, z1, 64),
		z0 = svrshr_x (p0, z1, 64))
