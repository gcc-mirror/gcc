/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rsra_1_s64_tied1:
**	srsra	z0\.d, z1\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rsra_1_s64_tied1, svint64_t,
		z0 = svrsra_n_s64 (z0, z1, 1),
		z0 = svrsra (z0, z1, 1))

/*
** rsra_1_s64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	srsra	z0\.d, \1, #1
**	ret
*/
TEST_UNIFORM_Z (rsra_1_s64_tied2, svint64_t,
		z0 = svrsra_n_s64 (z1, z0, 1),
		z0 = svrsra (z1, z0, 1))

/*
** rsra_1_s64_untied:
**	movprfx	z0, z1
**	srsra	z0\.d, z2\.d, #1
**	ret
*/
TEST_UNIFORM_Z (rsra_1_s64_untied, svint64_t,
		z0 = svrsra_n_s64 (z1, z2, 1),
		z0 = svrsra (z1, z2, 1))

/*
** rsra_2_s64_tied1:
**	srsra	z0\.d, z1\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rsra_2_s64_tied1, svint64_t,
		z0 = svrsra_n_s64 (z0, z1, 2),
		z0 = svrsra (z0, z1, 2))

/*
** rsra_2_s64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	srsra	z0\.d, \1, #2
**	ret
*/
TEST_UNIFORM_Z (rsra_2_s64_tied2, svint64_t,
		z0 = svrsra_n_s64 (z1, z0, 2),
		z0 = svrsra (z1, z0, 2))

/*
** rsra_2_s64_untied:
**	movprfx	z0, z1
**	srsra	z0\.d, z2\.d, #2
**	ret
*/
TEST_UNIFORM_Z (rsra_2_s64_untied, svint64_t,
		z0 = svrsra_n_s64 (z1, z2, 2),
		z0 = svrsra (z1, z2, 2))

/*
** rsra_64_s64_tied1:
**	srsra	z0\.d, z1\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rsra_64_s64_tied1, svint64_t,
		z0 = svrsra_n_s64 (z0, z1, 64),
		z0 = svrsra (z0, z1, 64))

/*
** rsra_64_s64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	srsra	z0\.d, \1, #64
**	ret
*/
TEST_UNIFORM_Z (rsra_64_s64_tied2, svint64_t,
		z0 = svrsra_n_s64 (z1, z0, 64),
		z0 = svrsra (z1, z0, 64))

/*
** rsra_64_s64_untied:
**	movprfx	z0, z1
**	srsra	z0\.d, z2\.d, #64
**	ret
*/
TEST_UNIFORM_Z (rsra_64_s64_untied, svint64_t,
		z0 = svrsra_n_s64 (z1, z2, 64),
		z0 = svrsra (z1, z2, 64))
