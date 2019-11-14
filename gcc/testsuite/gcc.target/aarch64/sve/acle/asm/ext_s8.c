/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ext_0_s8_tied1:
**	ext	z0\.b, z0\.b, z1\.b, #0
**	ret
*/
TEST_UNIFORM_Z (ext_0_s8_tied1, svint8_t,
		z0 = svext_s8 (z0, z1, 0),
		z0 = svext (z0, z1, 0))

/*
** ext_0_s8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, \1\.b, #0
**	ret
*/
TEST_UNIFORM_Z (ext_0_s8_tied2, svint8_t,
		z0 = svext_s8 (z1, z0, 0),
		z0 = svext (z1, z0, 0))

/*
** ext_0_s8_untied:
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, z2\.b, #0
**	ret
*/
TEST_UNIFORM_Z (ext_0_s8_untied, svint8_t,
		z0 = svext_s8 (z1, z2, 0),
		z0 = svext (z1, z2, 0))

/*
** ext_1_s8:
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, z2\.b, #1
**	ret
*/
TEST_UNIFORM_Z (ext_1_s8, svint8_t,
		z0 = svext_s8 (z1, z2, 1),
		z0 = svext (z1, z2, 1))

/*
** ext_2_s8:
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, z2\.b, #2
**	ret
*/
TEST_UNIFORM_Z (ext_2_s8, svint8_t,
		z0 = svext_s8 (z1, z2, 2),
		z0 = svext (z1, z2, 2))

/*
** ext_3_s8:
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, z2\.b, #3
**	ret
*/
TEST_UNIFORM_Z (ext_3_s8, svint8_t,
		z0 = svext_s8 (z1, z2, 3),
		z0 = svext (z1, z2, 3))

/*
** ext_255_s8:
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, z2\.b, #255
**	ret
*/
TEST_UNIFORM_Z (ext_255_s8, svint8_t,
		z0 = svext_s8 (z1, z2, 255),
		z0 = svext (z1, z2, 255))
