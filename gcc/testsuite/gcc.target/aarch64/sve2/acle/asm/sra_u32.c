/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sra_1_u32_tied1:
**	usra	z0\.s, z1\.s, #1
**	ret
*/
TEST_UNIFORM_Z (sra_1_u32_tied1, svuint32_t,
		z0 = svsra_n_u32 (z0, z1, 1),
		z0 = svsra (z0, z1, 1))

/*
** sra_1_u32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	usra	z0\.s, \1\.s, #1
**	ret
*/
TEST_UNIFORM_Z (sra_1_u32_tied2, svuint32_t,
		z0 = svsra_n_u32 (z1, z0, 1),
		z0 = svsra (z1, z0, 1))

/*
** sra_1_u32_untied:
**	movprfx	z0, z1
**	usra	z0\.s, z2\.s, #1
**	ret
*/
TEST_UNIFORM_Z (sra_1_u32_untied, svuint32_t,
		z0 = svsra_n_u32 (z1, z2, 1),
		z0 = svsra (z1, z2, 1))

/*
** sra_2_u32_tied1:
**	usra	z0\.s, z1\.s, #2
**	ret
*/
TEST_UNIFORM_Z (sra_2_u32_tied1, svuint32_t,
		z0 = svsra_n_u32 (z0, z1, 2),
		z0 = svsra (z0, z1, 2))

/*
** sra_2_u32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	usra	z0\.s, \1\.s, #2
**	ret
*/
TEST_UNIFORM_Z (sra_2_u32_tied2, svuint32_t,
		z0 = svsra_n_u32 (z1, z0, 2),
		z0 = svsra (z1, z0, 2))

/*
** sra_2_u32_untied:
**	movprfx	z0, z1
**	usra	z0\.s, z2\.s, #2
**	ret
*/
TEST_UNIFORM_Z (sra_2_u32_untied, svuint32_t,
		z0 = svsra_n_u32 (z1, z2, 2),
		z0 = svsra (z1, z2, 2))

/*
** sra_32_u32_tied1:
**	usra	z0\.s, z1\.s, #32
**	ret
*/
TEST_UNIFORM_Z (sra_32_u32_tied1, svuint32_t,
		z0 = svsra_n_u32 (z0, z1, 32),
		z0 = svsra (z0, z1, 32))

/*
** sra_32_u32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	usra	z0\.s, \1\.s, #32
**	ret
*/
TEST_UNIFORM_Z (sra_32_u32_tied2, svuint32_t,
		z0 = svsra_n_u32 (z1, z0, 32),
		z0 = svsra (z1, z0, 32))

/*
** sra_32_u32_untied:
**	movprfx	z0, z1
**	usra	z0\.s, z2\.s, #32
**	ret
*/
TEST_UNIFORM_Z (sra_32_u32_untied, svuint32_t,
		z0 = svsra_n_u32 (z1, z2, 32),
		z0 = svsra (z1, z2, 32))

/*
** sra_2_u32_zeroop1:
**	lsr	z0\.s, z1\.s, #2
**	ret
*/
TEST_UNIFORM_Z (sra_2_u32_zeroop1, svuint32_t,
		z0 = svsra_n_u32 (svdup_u32 (0), z1, 2),
		z0 = svsra (svdup_u32 (0), z1, 2))
