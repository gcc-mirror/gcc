/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sri_1_s8_tied1:
**	sri	z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (sri_1_s8_tied1, svint8_t,
		z0 = svsri_n_s8 (z0, z1, 1),
		z0 = svsri (z0, z1, 1))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_1_s8_tied2, svint8_t,
		z0 = svsri_n_s8 (z1, z0, 1),
		z0 = svsri (z1, z0, 1))

/*
** sri_1_s8_untied:
** (
**	mov	z0\.d, z1\.d
**	sri	z0\.b, z2\.b, #1
** |
**	sri	z1\.b, z2\.b, #1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sri_1_s8_untied, svint8_t,
		z0 = svsri_n_s8 (z1, z2, 1),
		z0 = svsri (z1, z2, 1))

/*
** sri_2_s8_tied1:
**	sri	z0\.b, z1\.b, #2
**	ret
*/
TEST_UNIFORM_Z (sri_2_s8_tied1, svint8_t,
		z0 = svsri_n_s8 (z0, z1, 2),
		z0 = svsri (z0, z1, 2))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_2_s8_tied2, svint8_t,
		z0 = svsri_n_s8 (z1, z0, 2),
		z0 = svsri (z1, z0, 2))

/*
** sri_2_s8_untied:
** (
**	mov	z0\.d, z1\.d
**	sri	z0\.b, z2\.b, #2
** |
**	sri	z1\.b, z2\.b, #2
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sri_2_s8_untied, svint8_t,
		z0 = svsri_n_s8 (z1, z2, 2),
		z0 = svsri (z1, z2, 2))

/*
** sri_8_s8_tied1:
**	sri	z0\.b, z1\.b, #8
**	ret
*/
TEST_UNIFORM_Z (sri_8_s8_tied1, svint8_t,
		z0 = svsri_n_s8 (z0, z1, 8),
		z0 = svsri (z0, z1, 8))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_8_s8_tied2, svint8_t,
		z0 = svsri_n_s8 (z1, z0, 8),
		z0 = svsri (z1, z0, 8))

/*
** sri_8_s8_untied:
** (
**	mov	z0\.d, z1\.d
**	sri	z0\.b, z2\.b, #8
** |
**	sri	z1\.b, z2\.b, #8
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sri_8_s8_untied, svint8_t,
		z0 = svsri_n_s8 (z1, z2, 8),
		z0 = svsri (z1, z2, 8))
