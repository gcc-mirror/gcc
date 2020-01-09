/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** xar_1_u64_tied1:
**	xar	z0\.d, z0\.d, z1\.d, #1
**	ret
*/
TEST_UNIFORM_Z (xar_1_u64_tied1, svuint64_t,
		z0 = svxar_n_u64 (z0, z1, 1),
		z0 = svxar (z0, z1, 1))

/*
** xar_1_u64_tied2:
**	xar	z0\.d, z0\.d, z1\.d, #1
**	ret
*/
TEST_UNIFORM_Z (xar_1_u64_tied2, svuint64_t,
		z0 = svxar_n_u64 (z1, z0, 1),
		z0 = svxar (z1, z0, 1))

/*
** xar_1_u64_untied:
** (
**	movprfx	z0, z1
**	xar	z0\.d, z0\.d, z2\.d, #1
** |
**	movprfx	z0, z2
**	xar	z0\.d, z0\.d, z1\.d, #1
** )
**	ret
*/
TEST_UNIFORM_Z (xar_1_u64_untied, svuint64_t,
		z0 = svxar_n_u64 (z1, z2, 1),
		z0 = svxar (z1, z2, 1))

/*
** xar_2_u64_tied1:
**	xar	z0\.d, z0\.d, z1\.d, #2
**	ret
*/
TEST_UNIFORM_Z (xar_2_u64_tied1, svuint64_t,
		z0 = svxar_n_u64 (z0, z1, 2),
		z0 = svxar (z0, z1, 2))

/*
** xar_2_u64_tied2:
**	xar	z0\.d, z0\.d, z1\.d, #2
**	ret
*/
TEST_UNIFORM_Z (xar_2_u64_tied2, svuint64_t,
		z0 = svxar_n_u64 (z1, z0, 2),
		z0 = svxar (z1, z0, 2))

/*
** xar_2_u64_untied:
** (
**	movprfx	z0, z1
**	xar	z0\.d, z0\.d, z2\.d, #2
** |
**	movprfx	z0, z2
**	xar	z0\.d, z0\.d, z1\.d, #2
** )
**	ret
*/
TEST_UNIFORM_Z (xar_2_u64_untied, svuint64_t,
		z0 = svxar_n_u64 (z1, z2, 2),
		z0 = svxar (z1, z2, 2))

/*
** xar_64_u64_tied1:
**	xar	z0\.d, z0\.d, z1\.d, #64
**	ret
*/
TEST_UNIFORM_Z (xar_64_u64_tied1, svuint64_t,
		z0 = svxar_n_u64 (z0, z1, 64),
		z0 = svxar (z0, z1, 64))

/*
** xar_64_u64_tied2:
**	xar	z0\.d, z0\.d, z1\.d, #64
**	ret
*/
TEST_UNIFORM_Z (xar_64_u64_tied2, svuint64_t,
		z0 = svxar_n_u64 (z1, z0, 64),
		z0 = svxar (z1, z0, 64))

/*
** xar_64_u64_untied:
** (
**	movprfx	z0, z1
**	xar	z0\.d, z0\.d, z2\.d, #64
** |
**	movprfx	z0, z2
**	xar	z0\.d, z0\.d, z1\.d, #64
** )
**	ret
*/
TEST_UNIFORM_Z (xar_64_u64_untied, svuint64_t,
		z0 = svxar_n_u64 (z1, z2, 64),
		z0 = svxar (z1, z2, 64))
