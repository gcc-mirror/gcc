/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** asrd_1_s16_m_tied1:
**	asrd	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s16_m_tied1, svint16_t,
		z0 = svasrd_n_s16_m (p0, z0, 1),
		z0 = svasrd_m (p0, z0, 1))

/*
** asrd_1_s16_m_untied:
**	movprfx	z0, z1
**	asrd	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s16_m_untied, svint16_t,
		z0 = svasrd_n_s16_m (p0, z1, 1),
		z0 = svasrd_m (p0, z1, 1))

/*
** asrd_2_s16_m_tied1:
**	asrd	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s16_m_tied1, svint16_t,
		z0 = svasrd_n_s16_m (p0, z0, 2),
		z0 = svasrd_m (p0, z0, 2))

/*
** asrd_2_s16_m_untied:
**	movprfx	z0, z1
**	asrd	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s16_m_untied, svint16_t,
		z0 = svasrd_n_s16_m (p0, z1, 2),
		z0 = svasrd_m (p0, z1, 2))

/*
** asrd_16_s16_m_tied1:
**	asrd	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (asrd_16_s16_m_tied1, svint16_t,
		z0 = svasrd_n_s16_m (p0, z0, 16),
		z0 = svasrd_m (p0, z0, 16))

/*
** asrd_16_s16_m_untied:
**	movprfx	z0, z1
**	asrd	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (asrd_16_s16_m_untied, svint16_t,
		z0 = svasrd_n_s16_m (p0, z1, 16),
		z0 = svasrd_m (p0, z1, 16))

/*
** asrd_1_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	asrd	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s16_z_tied1, svint16_t,
		z0 = svasrd_n_s16_z (p0, z0, 1),
		z0 = svasrd_z (p0, z0, 1))

/*
** asrd_1_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	asrd	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s16_z_untied, svint16_t,
		z0 = svasrd_n_s16_z (p0, z1, 1),
		z0 = svasrd_z (p0, z1, 1))

/*
** asrd_2_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	asrd	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s16_z_tied1, svint16_t,
		z0 = svasrd_n_s16_z (p0, z0, 2),
		z0 = svasrd_z (p0, z0, 2))

/*
** asrd_2_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	asrd	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s16_z_untied, svint16_t,
		z0 = svasrd_n_s16_z (p0, z1, 2),
		z0 = svasrd_z (p0, z1, 2))

/*
** asrd_16_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	asrd	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (asrd_16_s16_z_tied1, svint16_t,
		z0 = svasrd_n_s16_z (p0, z0, 16),
		z0 = svasrd_z (p0, z0, 16))

/*
** asrd_16_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	asrd	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (asrd_16_s16_z_untied, svint16_t,
		z0 = svasrd_n_s16_z (p0, z1, 16),
		z0 = svasrd_z (p0, z1, 16))

/*
** asrd_1_s16_x_tied1:
**	asrd	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s16_x_tied1, svint16_t,
		z0 = svasrd_n_s16_x (p0, z0, 1),
		z0 = svasrd_x (p0, z0, 1))

/*
** asrd_1_s16_x_untied:
**	movprfx	z0, z1
**	asrd	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s16_x_untied, svint16_t,
		z0 = svasrd_n_s16_x (p0, z1, 1),
		z0 = svasrd_x (p0, z1, 1))

/*
** asrd_2_s16_x_tied1:
**	asrd	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s16_x_tied1, svint16_t,
		z0 = svasrd_n_s16_x (p0, z0, 2),
		z0 = svasrd_x (p0, z0, 2))

/*
** asrd_2_s16_x_untied:
**	movprfx	z0, z1
**	asrd	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s16_x_untied, svint16_t,
		z0 = svasrd_n_s16_x (p0, z1, 2),
		z0 = svasrd_x (p0, z1, 2))

/*
** asrd_16_s16_x_tied1:
**	asrd	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (asrd_16_s16_x_tied1, svint16_t,
		z0 = svasrd_n_s16_x (p0, z0, 16),
		z0 = svasrd_x (p0, z0, 16))

/*
** asrd_16_s16_x_untied:
**	movprfx	z0, z1
**	asrd	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (asrd_16_s16_x_untied, svint16_t,
		z0 = svasrd_n_s16_x (p0, z1, 16),
		z0 = svasrd_x (p0, z1, 16))
