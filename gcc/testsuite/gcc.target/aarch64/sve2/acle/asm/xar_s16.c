/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** xar_1_s16_tied1:
**	xar	z0\.h, z0\.h, z1\.h, #1
**	ret
*/
TEST_UNIFORM_Z (xar_1_s16_tied1, svint16_t,
		z0 = svxar_n_s16 (z0, z1, 1),
		z0 = svxar (z0, z1, 1))

/*
** xar_1_s16_tied2:
**	xar	z0\.h, z0\.h, z1\.h, #1
**	ret
*/
TEST_UNIFORM_Z (xar_1_s16_tied2, svint16_t,
		z0 = svxar_n_s16 (z1, z0, 1),
		z0 = svxar (z1, z0, 1))

/*
** xar_1_s16_untied:
** (
**	movprfx	z0, z1
**	xar	z0\.h, z0\.h, z2\.h, #1
** |
**	movprfx	z0, z2
**	xar	z0\.h, z0\.h, z1\.h, #1
** )
**	ret
*/
TEST_UNIFORM_Z (xar_1_s16_untied, svint16_t,
		z0 = svxar_n_s16 (z1, z2, 1),
		z0 = svxar (z1, z2, 1))

/*
** xar_2_s16_tied1:
**	xar	z0\.h, z0\.h, z1\.h, #2
**	ret
*/
TEST_UNIFORM_Z (xar_2_s16_tied1, svint16_t,
		z0 = svxar_n_s16 (z0, z1, 2),
		z0 = svxar (z0, z1, 2))

/*
** xar_2_s16_tied2:
**	xar	z0\.h, z0\.h, z1\.h, #2
**	ret
*/
TEST_UNIFORM_Z (xar_2_s16_tied2, svint16_t,
		z0 = svxar_n_s16 (z1, z0, 2),
		z0 = svxar (z1, z0, 2))

/*
** xar_2_s16_untied:
** (
**	movprfx	z0, z1
**	xar	z0\.h, z0\.h, z2\.h, #2
** |
**	movprfx	z0, z2
**	xar	z0\.h, z0\.h, z1\.h, #2
** )
**	ret
*/
TEST_UNIFORM_Z (xar_2_s16_untied, svint16_t,
		z0 = svxar_n_s16 (z1, z2, 2),
		z0 = svxar (z1, z2, 2))

/*
** xar_16_s16_tied1:
**	xar	z0\.h, z0\.h, z1\.h, #16
**	ret
*/
TEST_UNIFORM_Z (xar_16_s16_tied1, svint16_t,
		z0 = svxar_n_s16 (z0, z1, 16),
		z0 = svxar (z0, z1, 16))

/*
** xar_16_s16_tied2:
**	xar	z0\.h, z0\.h, z1\.h, #16
**	ret
*/
TEST_UNIFORM_Z (xar_16_s16_tied2, svint16_t,
		z0 = svxar_n_s16 (z1, z0, 16),
		z0 = svxar (z1, z0, 16))

/*
** xar_16_s16_untied:
** (
**	movprfx	z0, z1
**	xar	z0\.h, z0\.h, z2\.h, #16
** |
**	movprfx	z0, z2
**	xar	z0\.h, z0\.h, z1\.h, #16
** )
**	ret
*/
TEST_UNIFORM_Z (xar_16_s16_untied, svint16_t,
		z0 = svxar_n_s16 (z1, z2, 16),
		z0 = svxar (z1, z2, 16))
