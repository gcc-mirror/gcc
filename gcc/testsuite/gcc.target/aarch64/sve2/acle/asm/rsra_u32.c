/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rsra_1_u32_tied1:
**	ursra	z0\.s, z1\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rsra_1_u32_tied1, svuint32_t,
		z0 = svrsra_n_u32 (z0, z1, 1),
		z0 = svrsra (z0, z1, 1))

/*
** rsra_1_u32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	ursra	z0\.s, \1\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rsra_1_u32_tied2, svuint32_t,
		z0 = svrsra_n_u32 (z1, z0, 1),
		z0 = svrsra (z1, z0, 1))

/*
** rsra_1_u32_untied:
**	movprfx	z0, z1
**	ursra	z0\.s, z2\.s, #1
**	ret
*/
TEST_UNIFORM_Z (rsra_1_u32_untied, svuint32_t,
		z0 = svrsra_n_u32 (z1, z2, 1),
		z0 = svrsra (z1, z2, 1))

/*
** rsra_2_u32_tied1:
**	ursra	z0\.s, z1\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rsra_2_u32_tied1, svuint32_t,
		z0 = svrsra_n_u32 (z0, z1, 2),
		z0 = svrsra (z0, z1, 2))

/*
** rsra_2_u32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	ursra	z0\.s, \1\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rsra_2_u32_tied2, svuint32_t,
		z0 = svrsra_n_u32 (z1, z0, 2),
		z0 = svrsra (z1, z0, 2))

/*
** rsra_2_u32_untied:
**	movprfx	z0, z1
**	ursra	z0\.s, z2\.s, #2
**	ret
*/
TEST_UNIFORM_Z (rsra_2_u32_untied, svuint32_t,
		z0 = svrsra_n_u32 (z1, z2, 2),
		z0 = svrsra (z1, z2, 2))

/*
** rsra_32_u32_tied1:
**	ursra	z0\.s, z1\.s, #32
**	ret
*/
TEST_UNIFORM_Z (rsra_32_u32_tied1, svuint32_t,
		z0 = svrsra_n_u32 (z0, z1, 32),
		z0 = svrsra (z0, z1, 32))

/*
** rsra_32_u32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	ursra	z0\.s, \1\.s, #32
**	ret
*/
TEST_UNIFORM_Z (rsra_32_u32_tied2, svuint32_t,
		z0 = svrsra_n_u32 (z1, z0, 32),
		z0 = svrsra (z1, z0, 32))

/*
** rsra_32_u32_untied:
**	movprfx	z0, z1
**	ursra	z0\.s, z2\.s, #32
**	ret
*/
TEST_UNIFORM_Z (rsra_32_u32_untied, svuint32_t,
		z0 = svrsra_n_u32 (z1, z2, 32),
		z0 = svrsra (z1, z2, 32))
