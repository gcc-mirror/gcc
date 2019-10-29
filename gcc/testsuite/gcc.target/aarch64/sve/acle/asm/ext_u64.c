/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ext_0_u64_tied1:
**	ext	z0\.b, z0\.b, z1\.b, #0
**	ret
*/
TEST_UNIFORM_Z (ext_0_u64_tied1, svuint64_t,
		z0 = svext_u64 (z0, z1, 0),
		z0 = svext (z0, z1, 0))

/*
** ext_0_u64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, \1\.b, #0
**	ret
*/
TEST_UNIFORM_Z (ext_0_u64_tied2, svuint64_t,
		z0 = svext_u64 (z1, z0, 0),
		z0 = svext (z1, z0, 0))

/*
** ext_0_u64_untied:
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, z2\.b, #0
**	ret
*/
TEST_UNIFORM_Z (ext_0_u64_untied, svuint64_t,
		z0 = svext_u64 (z1, z2, 0),
		z0 = svext (z1, z2, 0))

/*
** ext_1_u64:
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, z2\.b, #8
**	ret
*/
TEST_UNIFORM_Z (ext_1_u64, svuint64_t,
		z0 = svext_u64 (z1, z2, 1),
		z0 = svext (z1, z2, 1))

/*
** ext_2_u64:
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, z2\.b, #16
**	ret
*/
TEST_UNIFORM_Z (ext_2_u64, svuint64_t,
		z0 = svext_u64 (z1, z2, 2),
		z0 = svext (z1, z2, 2))

/*
** ext_3_u64:
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, z2\.b, #24
**	ret
*/
TEST_UNIFORM_Z (ext_3_u64, svuint64_t,
		z0 = svext_u64 (z1, z2, 3),
		z0 = svext (z1, z2, 3))

/*
** ext_31_u64:
**	movprfx	z0, z1
**	ext	z0\.b, z0\.b, z2\.b, #248
**	ret
*/
TEST_UNIFORM_Z (ext_31_u64, svuint64_t,
		z0 = svext_u64 (z1, z2, 31),
		z0 = svext (z1, z2, 31))
