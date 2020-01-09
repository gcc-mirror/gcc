/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rsra_1_s8_tied1:
**	srsra	z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rsra_1_s8_tied1, svint8_t,
		z0 = svrsra_n_s8 (z0, z1, 1),
		z0 = svrsra (z0, z1, 1))

/*
** rsra_1_s8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	srsra	z0\.b, \1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rsra_1_s8_tied2, svint8_t,
		z0 = svrsra_n_s8 (z1, z0, 1),
		z0 = svrsra (z1, z0, 1))

/*
** rsra_1_s8_untied:
**	movprfx	z0, z1
**	srsra	z0\.b, z2\.b, #1
**	ret
*/
TEST_UNIFORM_Z (rsra_1_s8_untied, svint8_t,
		z0 = svrsra_n_s8 (z1, z2, 1),
		z0 = svrsra (z1, z2, 1))

/*
** rsra_2_s8_tied1:
**	srsra	z0\.b, z1\.b, #2
**	ret
*/
TEST_UNIFORM_Z (rsra_2_s8_tied1, svint8_t,
		z0 = svrsra_n_s8 (z0, z1, 2),
		z0 = svrsra (z0, z1, 2))

/*
** rsra_2_s8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	srsra	z0\.b, \1\.b, #2
**	ret
*/
TEST_UNIFORM_Z (rsra_2_s8_tied2, svint8_t,
		z0 = svrsra_n_s8 (z1, z0, 2),
		z0 = svrsra (z1, z0, 2))

/*
** rsra_2_s8_untied:
**	movprfx	z0, z1
**	srsra	z0\.b, z2\.b, #2
**	ret
*/
TEST_UNIFORM_Z (rsra_2_s8_untied, svint8_t,
		z0 = svrsra_n_s8 (z1, z2, 2),
		z0 = svrsra (z1, z2, 2))

/*
** rsra_8_s8_tied1:
**	srsra	z0\.b, z1\.b, #8
**	ret
*/
TEST_UNIFORM_Z (rsra_8_s8_tied1, svint8_t,
		z0 = svrsra_n_s8 (z0, z1, 8),
		z0 = svrsra (z0, z1, 8))

/*
** rsra_8_s8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	srsra	z0\.b, \1\.b, #8
**	ret
*/
TEST_UNIFORM_Z (rsra_8_s8_tied2, svint8_t,
		z0 = svrsra_n_s8 (z1, z0, 8),
		z0 = svrsra (z1, z0, 8))

/*
** rsra_8_s8_untied:
**	movprfx	z0, z1
**	srsra	z0\.b, z2\.b, #8
**	ret
*/
TEST_UNIFORM_Z (rsra_8_s8_untied, svint8_t,
		z0 = svrsra_n_s8 (z1, z2, 8),
		z0 = svrsra (z1, z2, 8))
