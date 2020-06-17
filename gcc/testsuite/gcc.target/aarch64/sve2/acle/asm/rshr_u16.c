/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshr_1_u16_m_tied1:
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_u16_m_tied1, svuint16_t,
		z0 = svrshr_n_u16_m (p0, z0, 1),
		z0 = svrshr_m (p0, z0, 1))

/*
** rshr_1_u16_m_untied:
**	movprfx	z0, z1
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_u16_m_untied, svuint16_t,
		z0 = svrshr_n_u16_m (p0, z1, 1),
		z0 = svrshr_m (p0, z1, 1))

/*
** rshr_2_u16_m_tied1:
**	urshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_u16_m_tied1, svuint16_t,
		z0 = svrshr_n_u16_m (p0, z0, 2),
		z0 = svrshr_m (p0, z0, 2))

/*
** rshr_2_u16_m_untied:
**	movprfx	z0, z1
**	urshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_u16_m_untied, svuint16_t,
		z0 = svrshr_n_u16_m (p0, z1, 2),
		z0 = svrshr_m (p0, z1, 2))

/*
** rshr_16_u16_m_tied1:
**	urshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (rshr_16_u16_m_tied1, svuint16_t,
		z0 = svrshr_n_u16_m (p0, z0, 16),
		z0 = svrshr_m (p0, z0, 16))

/*
** rshr_16_u16_m_untied:
**	movprfx	z0, z1
**	urshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (rshr_16_u16_m_untied, svuint16_t,
		z0 = svrshr_n_u16_m (p0, z1, 16),
		z0 = svrshr_m (p0, z1, 16))

/*
** rshr_1_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_u16_z_tied1, svuint16_t,
		z0 = svrshr_n_u16_z (p0, z0, 1),
		z0 = svrshr_z (p0, z0, 1))

/*
** rshr_1_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_u16_z_untied, svuint16_t,
		z0 = svrshr_n_u16_z (p0, z1, 1),
		z0 = svrshr_z (p0, z1, 1))

/*
** rshr_2_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	urshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_u16_z_tied1, svuint16_t,
		z0 = svrshr_n_u16_z (p0, z0, 2),
		z0 = svrshr_z (p0, z0, 2))

/*
** rshr_2_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	urshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_u16_z_untied, svuint16_t,
		z0 = svrshr_n_u16_z (p0, z1, 2),
		z0 = svrshr_z (p0, z1, 2))

/*
** rshr_16_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	urshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (rshr_16_u16_z_tied1, svuint16_t,
		z0 = svrshr_n_u16_z (p0, z0, 16),
		z0 = svrshr_z (p0, z0, 16))

/*
** rshr_16_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	urshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (rshr_16_u16_z_untied, svuint16_t,
		z0 = svrshr_n_u16_z (p0, z1, 16),
		z0 = svrshr_z (p0, z1, 16))

/*
** rshr_1_u16_x_tied1:
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_u16_x_tied1, svuint16_t,
		z0 = svrshr_n_u16_x (p0, z0, 1),
		z0 = svrshr_x (p0, z0, 1))

/*
** rshr_1_u16_x_untied:
**	movprfx	z0, z1
**	urshr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (rshr_1_u16_x_untied, svuint16_t,
		z0 = svrshr_n_u16_x (p0, z1, 1),
		z0 = svrshr_x (p0, z1, 1))

/*
** rshr_2_u16_x_tied1:
**	urshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_u16_x_tied1, svuint16_t,
		z0 = svrshr_n_u16_x (p0, z0, 2),
		z0 = svrshr_x (p0, z0, 2))

/*
** rshr_2_u16_x_untied:
**	movprfx	z0, z1
**	urshr	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (rshr_2_u16_x_untied, svuint16_t,
		z0 = svrshr_n_u16_x (p0, z1, 2),
		z0 = svrshr_x (p0, z1, 2))

/*
** rshr_16_u16_x_tied1:
**	urshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (rshr_16_u16_x_tied1, svuint16_t,
		z0 = svrshr_n_u16_x (p0, z0, 16),
		z0 = svrshr_x (p0, z0, 16))

/*
** rshr_16_u16_x_untied:
**	movprfx	z0, z1
**	urshr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (rshr_16_u16_x_untied, svuint16_t,
		z0 = svrshr_n_u16_x (p0, z1, 16),
		z0 = svrshr_x (p0, z1, 16))
