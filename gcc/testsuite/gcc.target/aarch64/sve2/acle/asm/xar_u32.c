/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** xar_1_u32_tied1:
**	xar	z0\.s, z0\.s, z1\.s, #1
**	ret
*/
TEST_UNIFORM_Z (xar_1_u32_tied1, svuint32_t,
		z0 = svxar_n_u32 (z0, z1, 1),
		z0 = svxar (z0, z1, 1))

/*
** xar_1_u32_tied2:
**	xar	z0\.s, z0\.s, z1\.s, #1
**	ret
*/
TEST_UNIFORM_Z (xar_1_u32_tied2, svuint32_t,
		z0 = svxar_n_u32 (z1, z0, 1),
		z0 = svxar (z1, z0, 1))

/*
** xar_1_u32_untied:
** (
**	movprfx	z0, z1
**	xar	z0\.s, z0\.s, z2\.s, #1
** |
**	movprfx	z0, z2
**	xar	z0\.s, z0\.s, z1\.s, #1
** )
**	ret
*/
TEST_UNIFORM_Z (xar_1_u32_untied, svuint32_t,
		z0 = svxar_n_u32 (z1, z2, 1),
		z0 = svxar (z1, z2, 1))

/*
** xar_2_u32_tied1:
**	xar	z0\.s, z0\.s, z1\.s, #2
**	ret
*/
TEST_UNIFORM_Z (xar_2_u32_tied1, svuint32_t,
		z0 = svxar_n_u32 (z0, z1, 2),
		z0 = svxar (z0, z1, 2))

/*
** xar_2_u32_tied2:
**	xar	z0\.s, z0\.s, z1\.s, #2
**	ret
*/
TEST_UNIFORM_Z (xar_2_u32_tied2, svuint32_t,
		z0 = svxar_n_u32 (z1, z0, 2),
		z0 = svxar (z1, z0, 2))

/*
** xar_2_u32_untied:
** (
**	movprfx	z0, z1
**	xar	z0\.s, z0\.s, z2\.s, #2
** |
**	movprfx	z0, z2
**	xar	z0\.s, z0\.s, z1\.s, #2
** )
**	ret
*/
TEST_UNIFORM_Z (xar_2_u32_untied, svuint32_t,
		z0 = svxar_n_u32 (z1, z2, 2),
		z0 = svxar (z1, z2, 2))

/*
** xar_32_u32_tied1:
** (
**	eor	z0\.d, z1\.d, z0\.d
** |
**	eor	z0\.d, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (xar_32_u32_tied1, svuint32_t,
		z0 = svxar_n_u32 (z0, z1, 32),
		z0 = svxar (z0, z1, 32))

/*
** xar_32_u32_tied2:
** (
**	eor	z0\.d, z1\.d, z0\.d
** |
**	eor	z0\.d, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (xar_32_u32_tied2, svuint32_t,
		z0 = svxar_n_u32 (z1, z0, 32),
		z0 = svxar (z1, z0, 32))

/*
** xar_32_u32_untied:
** (
**	eor	z0\.d, z1\.d, z2\.d
** |
**	eor	z0\.d, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (xar_32_u32_untied, svuint32_t,
		z0 = svxar_n_u32 (z1, z2, 32),
		z0 = svxar (z1, z2, 32))
