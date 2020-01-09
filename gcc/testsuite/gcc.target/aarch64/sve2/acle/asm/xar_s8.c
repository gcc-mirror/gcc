/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** xar_1_s8_tied1:
**	xar	z0\.b, z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (xar_1_s8_tied1, svint8_t,
		z0 = svxar_n_s8 (z0, z1, 1),
		z0 = svxar (z0, z1, 1))

/*
** xar_1_s8_tied2:
**	xar	z0\.b, z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (xar_1_s8_tied2, svint8_t,
		z0 = svxar_n_s8 (z1, z0, 1),
		z0 = svxar (z1, z0, 1))

/*
** xar_1_s8_untied:
** (
**	movprfx	z0, z1
**	xar	z0\.b, z0\.b, z2\.b, #1
** |
**	movprfx	z0, z2
**	xar	z0\.b, z0\.b, z1\.b, #1
** )
**	ret
*/
TEST_UNIFORM_Z (xar_1_s8_untied, svint8_t,
		z0 = svxar_n_s8 (z1, z2, 1),
		z0 = svxar (z1, z2, 1))

/*
** xar_2_s8_tied1:
**	xar	z0\.b, z0\.b, z1\.b, #2
**	ret
*/
TEST_UNIFORM_Z (xar_2_s8_tied1, svint8_t,
		z0 = svxar_n_s8 (z0, z1, 2),
		z0 = svxar (z0, z1, 2))

/*
** xar_2_s8_tied2:
**	xar	z0\.b, z0\.b, z1\.b, #2
**	ret
*/
TEST_UNIFORM_Z (xar_2_s8_tied2, svint8_t,
		z0 = svxar_n_s8 (z1, z0, 2),
		z0 = svxar (z1, z0, 2))

/*
** xar_2_s8_untied:
** (
**	movprfx	z0, z1
**	xar	z0\.b, z0\.b, z2\.b, #2
** |
**	movprfx	z0, z2
**	xar	z0\.b, z0\.b, z1\.b, #2
** )
**	ret
*/
TEST_UNIFORM_Z (xar_2_s8_untied, svint8_t,
		z0 = svxar_n_s8 (z1, z2, 2),
		z0 = svxar (z1, z2, 2))

/*
** xar_8_s8_tied1:
**	xar	z0\.b, z0\.b, z1\.b, #8
**	ret
*/
TEST_UNIFORM_Z (xar_8_s8_tied1, svint8_t,
		z0 = svxar_n_s8 (z0, z1, 8),
		z0 = svxar (z0, z1, 8))

/*
** xar_8_s8_tied2:
**	xar	z0\.b, z0\.b, z1\.b, #8
**	ret
*/
TEST_UNIFORM_Z (xar_8_s8_tied2, svint8_t,
		z0 = svxar_n_s8 (z1, z0, 8),
		z0 = svxar (z1, z0, 8))

/*
** xar_8_s8_untied:
** (
**	movprfx	z0, z1
**	xar	z0\.b, z0\.b, z2\.b, #8
** |
**	movprfx	z0, z2
**	xar	z0\.b, z0\.b, z1\.b, #8
** )
**	ret
*/
TEST_UNIFORM_Z (xar_8_s8_untied, svint8_t,
		z0 = svxar_n_s8 (z1, z2, 8),
		z0 = svxar (z1, z2, 8))
