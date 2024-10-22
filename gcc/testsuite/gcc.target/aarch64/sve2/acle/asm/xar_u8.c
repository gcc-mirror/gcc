/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** xar_1_u8_tied1:
**	xar	z0\.b, z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (xar_1_u8_tied1, svuint8_t,
		z0 = svxar_n_u8 (z0, z1, 1),
		z0 = svxar (z0, z1, 1))

/*
** xar_1_u8_tied2:
**	xar	z0\.b, z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (xar_1_u8_tied2, svuint8_t,
		z0 = svxar_n_u8 (z1, z0, 1),
		z0 = svxar (z1, z0, 1))

/*
** xar_1_u8_untied:
** (
**	movprfx	z0, z1
**	xar	z0\.b, z0\.b, z2\.b, #1
** |
**	movprfx	z0, z2
**	xar	z0\.b, z0\.b, z1\.b, #1
** )
**	ret
*/
TEST_UNIFORM_Z (xar_1_u8_untied, svuint8_t,
		z0 = svxar_n_u8 (z1, z2, 1),
		z0 = svxar (z1, z2, 1))

/*
** xar_2_u8_tied1:
**	xar	z0\.b, z0\.b, z1\.b, #2
**	ret
*/
TEST_UNIFORM_Z (xar_2_u8_tied1, svuint8_t,
		z0 = svxar_n_u8 (z0, z1, 2),
		z0 = svxar (z0, z1, 2))

/*
** xar_2_u8_tied2:
**	xar	z0\.b, z0\.b, z1\.b, #2
**	ret
*/
TEST_UNIFORM_Z (xar_2_u8_tied2, svuint8_t,
		z0 = svxar_n_u8 (z1, z0, 2),
		z0 = svxar (z1, z0, 2))

/*
** xar_2_u8_untied:
** (
**	movprfx	z0, z1
**	xar	z0\.b, z0\.b, z2\.b, #2
** |
**	movprfx	z0, z2
**	xar	z0\.b, z0\.b, z1\.b, #2
** )
**	ret
*/
TEST_UNIFORM_Z (xar_2_u8_untied, svuint8_t,
		z0 = svxar_n_u8 (z1, z2, 2),
		z0 = svxar (z1, z2, 2))

/*
** xar_8_u8_tied1:
** (
**	eor	z0\.d, z1\.d, z0\.d
** |
**	eor	z0\.d, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (xar_8_u8_tied1, svuint8_t,
		z0 = svxar_n_u8 (z0, z1, 8),
		z0 = svxar (z0, z1, 8))

/*
** xar_8_u8_tied2:
** (
**	eor	z0\.d, z1\.d, z0\.d
** |
**	eor	z0\.d, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (xar_8_u8_tied2, svuint8_t,
		z0 = svxar_n_u8 (z1, z0, 8),
		z0 = svxar (z1, z0, 8))

/*
** xar_8_u8_untied:
** (
**	eor	z0\.d, z1\.d, z2\.d
** |
**	eor	z0\.d, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (xar_8_u8_untied, svuint8_t,
		z0 = svxar_n_u8 (z1, z2, 8),
		z0 = svxar (z1, z2, 8))
