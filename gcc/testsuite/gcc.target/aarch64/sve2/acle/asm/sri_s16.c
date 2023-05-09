/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sri_1_s16_tied1:
**	sri	z0\.h, z1\.h, #1
**	ret
*/
TEST_UNIFORM_Z (sri_1_s16_tied1, svint16_t,
		z0 = svsri_n_s16 (z0, z1, 1),
		z0 = svsri (z0, z1, 1))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_1_s16_tied2, svint16_t,
		z0 = svsri_n_s16 (z1, z0, 1),
		z0 = svsri (z1, z0, 1))

/*
** sri_1_s16_untied:
** (
**	mov	z0\.d, z1\.d
**	sri	z0\.h, z2\.h, #1
** |
**	sri	z1\.h, z2\.h, #1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sri_1_s16_untied, svint16_t,
		z0 = svsri_n_s16 (z1, z2, 1),
		z0 = svsri (z1, z2, 1))

/*
** sri_2_s16_tied1:
**	sri	z0\.h, z1\.h, #2
**	ret
*/
TEST_UNIFORM_Z (sri_2_s16_tied1, svint16_t,
		z0 = svsri_n_s16 (z0, z1, 2),
		z0 = svsri (z0, z1, 2))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_2_s16_tied2, svint16_t,
		z0 = svsri_n_s16 (z1, z0, 2),
		z0 = svsri (z1, z0, 2))

/*
** sri_2_s16_untied:
** (
**	mov	z0\.d, z1\.d
**	sri	z0\.h, z2\.h, #2
** |
**	sri	z1\.h, z2\.h, #2
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sri_2_s16_untied, svint16_t,
		z0 = svsri_n_s16 (z1, z2, 2),
		z0 = svsri (z1, z2, 2))

/*
** sri_16_s16_tied1:
**	sri	z0\.h, z1\.h, #16
**	ret
*/
TEST_UNIFORM_Z (sri_16_s16_tied1, svint16_t,
		z0 = svsri_n_s16 (z0, z1, 16),
		z0 = svsri (z0, z1, 16))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_16_s16_tied2, svint16_t,
		z0 = svsri_n_s16 (z1, z0, 16),
		z0 = svsri (z1, z0, 16))

/*
** sri_16_s16_untied:
** (
**	mov	z0\.d, z1\.d
**	sri	z0\.h, z2\.h, #16
** |
**	sri	z1\.h, z2\.h, #16
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sri_16_s16_untied, svint16_t,
		z0 = svsri_n_s16 (z1, z2, 16),
		z0 = svsri (z1, z2, 16))
