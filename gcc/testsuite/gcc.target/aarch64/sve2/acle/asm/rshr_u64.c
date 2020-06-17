/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshr_1_u64_m_tied1:
**	urshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_u64_m_tied1, svuint64_t,
		z0 = svrshr_n_u64_m (p0, z0, 1),
		z0 = svrshr_m (p0, z0, 1))

/*
** rshr_1_u64_m_untied:
**	movprfx	z0, z1
**	urshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_u64_m_untied, svuint64_t,
		z0 = svrshr_n_u64_m (p0, z1, 1),
		z0 = svrshr_m (p0, z1, 1))

/*
** rshr_2_u64_m_tied1:
**	urshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_u64_m_tied1, svuint64_t,
		z0 = svrshr_n_u64_m (p0, z0, 2),
		z0 = svrshr_m (p0, z0, 2))

/*
** rshr_2_u64_m_untied:
**	movprfx	z0, z1
**	urshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_u64_m_untied, svuint64_t,
		z0 = svrshr_n_u64_m (p0, z1, 2),
		z0 = svrshr_m (p0, z1, 2))

/*
** rshr_64_u64_m_tied1:
**	urshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rshr_64_u64_m_tied1, svuint64_t,
		z0 = svrshr_n_u64_m (p0, z0, 64),
		z0 = svrshr_m (p0, z0, 64))

/*
** rshr_64_u64_m_untied:
**	movprfx	z0, z1
**	urshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rshr_64_u64_m_untied, svuint64_t,
		z0 = svrshr_n_u64_m (p0, z1, 64),
		z0 = svrshr_m (p0, z1, 64))

/*
** rshr_1_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	urshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_u64_z_tied1, svuint64_t,
		z0 = svrshr_n_u64_z (p0, z0, 1),
		z0 = svrshr_z (p0, z0, 1))

/*
** rshr_1_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	urshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_u64_z_untied, svuint64_t,
		z0 = svrshr_n_u64_z (p0, z1, 1),
		z0 = svrshr_z (p0, z1, 1))

/*
** rshr_2_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	urshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_u64_z_tied1, svuint64_t,
		z0 = svrshr_n_u64_z (p0, z0, 2),
		z0 = svrshr_z (p0, z0, 2))

/*
** rshr_2_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	urshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_u64_z_untied, svuint64_t,
		z0 = svrshr_n_u64_z (p0, z1, 2),
		z0 = svrshr_z (p0, z1, 2))

/*
** rshr_64_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	urshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rshr_64_u64_z_tied1, svuint64_t,
		z0 = svrshr_n_u64_z (p0, z0, 64),
		z0 = svrshr_z (p0, z0, 64))

/*
** rshr_64_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	urshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rshr_64_u64_z_untied, svuint64_t,
		z0 = svrshr_n_u64_z (p0, z1, 64),
		z0 = svrshr_z (p0, z1, 64))

/*
** rshr_1_u64_x_tied1:
**	urshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_u64_x_tied1, svuint64_t,
		z0 = svrshr_n_u64_x (p0, z0, 1),
		z0 = svrshr_x (p0, z0, 1))

/*
** rshr_1_u64_x_untied:
**	movprfx	z0, z1
**	urshr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_u64_x_untied, svuint64_t,
		z0 = svrshr_n_u64_x (p0, z1, 1),
		z0 = svrshr_x (p0, z1, 1))

/*
** rshr_2_u64_x_tied1:
**	urshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_u64_x_tied1, svuint64_t,
		z0 = svrshr_n_u64_x (p0, z0, 2),
		z0 = svrshr_x (p0, z0, 2))

/*
** rshr_2_u64_x_untied:
**	movprfx	z0, z1
**	urshr	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_u64_x_untied, svuint64_t,
		z0 = svrshr_n_u64_x (p0, z1, 2),
		z0 = svrshr_x (p0, z1, 2))

/*
** rshr_64_u64_x_tied1:
**	urshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rshr_64_u64_x_tied1, svuint64_t,
		z0 = svrshr_n_u64_x (p0, z0, 64),
		z0 = svrshr_x (p0, z0, 64))

/*
** rshr_64_u64_x_untied:
**	movprfx	z0, z1
**	urshr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rshr_64_u64_x_untied, svuint64_t,
		z0 = svrshr_n_u64_x (p0, z1, 64),
		z0 = svrshr_x (p0, z1, 64))
