/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** asrd_1_s32_m_tied1:
**	asrd	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s32_m_tied1, svint32_t,
		z0 = svasrd_n_s32_m (p0, z0, 1),
		z0 = svasrd_m (p0, z0, 1))

/*
** asrd_1_s32_m_untied:
**	movprfx	z0, z1
**	asrd	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s32_m_untied, svint32_t,
		z0 = svasrd_n_s32_m (p0, z1, 1),
		z0 = svasrd_m (p0, z1, 1))

/*
** asrd_2_s32_m_tied1:
**	asrd	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s32_m_tied1, svint32_t,
		z0 = svasrd_n_s32_m (p0, z0, 2),
		z0 = svasrd_m (p0, z0, 2))

/*
** asrd_2_s32_m_untied:
**	movprfx	z0, z1
**	asrd	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s32_m_untied, svint32_t,
		z0 = svasrd_n_s32_m (p0, z1, 2),
		z0 = svasrd_m (p0, z1, 2))

/*
** asrd_32_s32_m_tied1:
**	asrd	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (asrd_32_s32_m_tied1, svint32_t,
		z0 = svasrd_n_s32_m (p0, z0, 32),
		z0 = svasrd_m (p0, z0, 32))

/*
** asrd_32_s32_m_untied:
**	movprfx	z0, z1
**	asrd	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (asrd_32_s32_m_untied, svint32_t,
		z0 = svasrd_n_s32_m (p0, z1, 32),
		z0 = svasrd_m (p0, z1, 32))

/*
** asrd_1_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	asrd	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s32_z_tied1, svint32_t,
		z0 = svasrd_n_s32_z (p0, z0, 1),
		z0 = svasrd_z (p0, z0, 1))

/*
** asrd_1_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	asrd	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s32_z_untied, svint32_t,
		z0 = svasrd_n_s32_z (p0, z1, 1),
		z0 = svasrd_z (p0, z1, 1))

/*
** asrd_2_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	asrd	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s32_z_tied1, svint32_t,
		z0 = svasrd_n_s32_z (p0, z0, 2),
		z0 = svasrd_z (p0, z0, 2))

/*
** asrd_2_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	asrd	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s32_z_untied, svint32_t,
		z0 = svasrd_n_s32_z (p0, z1, 2),
		z0 = svasrd_z (p0, z1, 2))

/*
** asrd_32_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	asrd	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (asrd_32_s32_z_tied1, svint32_t,
		z0 = svasrd_n_s32_z (p0, z0, 32),
		z0 = svasrd_z (p0, z0, 32))

/*
** asrd_32_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	asrd	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (asrd_32_s32_z_untied, svint32_t,
		z0 = svasrd_n_s32_z (p0, z1, 32),
		z0 = svasrd_z (p0, z1, 32))

/*
** asrd_1_s32_x_tied1:
**	asrd	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s32_x_tied1, svint32_t,
		z0 = svasrd_n_s32_x (p0, z0, 1),
		z0 = svasrd_x (p0, z0, 1))

/*
** asrd_1_s32_x_untied:
**	movprfx	z0, z1
**	asrd	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (asrd_1_s32_x_untied, svint32_t,
		z0 = svasrd_n_s32_x (p0, z1, 1),
		z0 = svasrd_x (p0, z1, 1))

/*
** asrd_2_s32_x_tied1:
**	asrd	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s32_x_tied1, svint32_t,
		z0 = svasrd_n_s32_x (p0, z0, 2),
		z0 = svasrd_x (p0, z0, 2))

/*
** asrd_2_s32_x_untied:
**	movprfx	z0, z1
**	asrd	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (asrd_2_s32_x_untied, svint32_t,
		z0 = svasrd_n_s32_x (p0, z1, 2),
		z0 = svasrd_x (p0, z1, 2))

/*
** asrd_32_s32_x_tied1:
**	asrd	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (asrd_32_s32_x_tied1, svint32_t,
		z0 = svasrd_n_s32_x (p0, z0, 32),
		z0 = svasrd_x (p0, z0, 32))

/*
** asrd_32_s32_x_untied:
**	movprfx	z0, z1
**	asrd	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (asrd_32_s32_x_untied, svint32_t,
		z0 = svasrd_n_s32_x (p0, z1, 32),
		z0 = svasrd_x (p0, z1, 32))
