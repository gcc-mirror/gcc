/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** asrd_1_s8_m_tied1:
**	asrd	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s8_m_tied1, svint8_t,
		z0 = svasrd_n_s8_m (p0, z0, 1),
		z0 = svasrd_m (p0, z0, 1))

/*
** asrd_1_s8_m_untied:
**	movprfx	z0, z1
**	asrd	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s8_m_untied, svint8_t,
		z0 = svasrd_n_s8_m (p0, z1, 1),
		z0 = svasrd_m (p0, z1, 1))

/*
** asrd_2_s8_m_tied1:
**	asrd	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s8_m_tied1, svint8_t,
		z0 = svasrd_n_s8_m (p0, z0, 2),
		z0 = svasrd_m (p0, z0, 2))

/*
** asrd_2_s8_m_untied:
**	movprfx	z0, z1
**	asrd	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s8_m_untied, svint8_t,
		z0 = svasrd_n_s8_m (p0, z1, 2),
		z0 = svasrd_m (p0, z1, 2))

/*
** asrd_8_s8_m_tied1:
**	asrd	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asrd_8_s8_m_tied1, svint8_t,
		z0 = svasrd_n_s8_m (p0, z0, 8),
		z0 = svasrd_m (p0, z0, 8))

/*
** asrd_8_s8_m_untied:
**	movprfx	z0, z1
**	asrd	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asrd_8_s8_m_untied, svint8_t,
		z0 = svasrd_n_s8_m (p0, z1, 8),
		z0 = svasrd_m (p0, z1, 8))

/*
** asrd_1_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	asrd	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s8_z_tied1, svint8_t,
		z0 = svasrd_n_s8_z (p0, z0, 1),
		z0 = svasrd_z (p0, z0, 1))

/*
** asrd_1_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	asrd	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s8_z_untied, svint8_t,
		z0 = svasrd_n_s8_z (p0, z1, 1),
		z0 = svasrd_z (p0, z1, 1))

/*
** asrd_2_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	asrd	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s8_z_tied1, svint8_t,
		z0 = svasrd_n_s8_z (p0, z0, 2),
		z0 = svasrd_z (p0, z0, 2))

/*
** asrd_2_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	asrd	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s8_z_untied, svint8_t,
		z0 = svasrd_n_s8_z (p0, z1, 2),
		z0 = svasrd_z (p0, z1, 2))

/*
** asrd_8_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	asrd	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asrd_8_s8_z_tied1, svint8_t,
		z0 = svasrd_n_s8_z (p0, z0, 8),
		z0 = svasrd_z (p0, z0, 8))

/*
** asrd_8_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	asrd	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asrd_8_s8_z_untied, svint8_t,
		z0 = svasrd_n_s8_z (p0, z1, 8),
		z0 = svasrd_z (p0, z1, 8))

/*
** asrd_1_s8_x_tied1:
**	asrd	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s8_x_tied1, svint8_t,
		z0 = svasrd_n_s8_x (p0, z0, 1),
		z0 = svasrd_x (p0, z0, 1))

/*
** asrd_1_s8_x_untied:
**	movprfx	z0, z1
**	asrd	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s8_x_untied, svint8_t,
		z0 = svasrd_n_s8_x (p0, z1, 1),
		z0 = svasrd_x (p0, z1, 1))

/*
** asrd_2_s8_x_tied1:
**	asrd	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s8_x_tied1, svint8_t,
		z0 = svasrd_n_s8_x (p0, z0, 2),
		z0 = svasrd_x (p0, z0, 2))

/*
** asrd_2_s8_x_untied:
**	movprfx	z0, z1
**	asrd	z0\.b, p0/m, z0\.b, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s8_x_untied, svint8_t,
		z0 = svasrd_n_s8_x (p0, z1, 2),
		z0 = svasrd_x (p0, z1, 2))

/*
** asrd_8_s8_x_tied1:
**	asrd	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asrd_8_s8_x_tied1, svint8_t,
		z0 = svasrd_n_s8_x (p0, z0, 8),
		z0 = svasrd_x (p0, z0, 8))

/*
** asrd_8_s8_x_untied:
**	movprfx	z0, z1
**	asrd	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asrd_8_s8_x_untied, svint8_t,
		z0 = svasrd_n_s8_x (p0, z1, 8),
		z0 = svasrd_x (p0, z1, 8))
