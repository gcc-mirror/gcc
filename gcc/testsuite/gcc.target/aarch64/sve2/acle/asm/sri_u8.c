/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sri_1_u8_tied1:
**	sri	z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (sri_1_u8_tied1, svuint8_t,
		z0 = svsri_n_u8 (z0, z1, 1),
		z0 = svsri (z0, z1, 1))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_1_u8_tied2, svuint8_t,
		z0 = svsri_n_u8 (z1, z0, 1),
		z0 = svsri (z1, z0, 1))

/*
** sri_1_u8_untied:
**	mov	z0\.d, z1\.d
**	sri	z0\.b, z2\.b, #1
**	ret
*/
TEST_UNIFORM_Z (sri_1_u8_untied, svuint8_t,
		z0 = svsri_n_u8 (z1, z2, 1),
		z0 = svsri (z1, z2, 1))

/*
** sri_2_u8_tied1:
**	sri	z0\.b, z1\.b, #2
**	ret
*/
TEST_UNIFORM_Z (sri_2_u8_tied1, svuint8_t,
		z0 = svsri_n_u8 (z0, z1, 2),
		z0 = svsri (z0, z1, 2))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_2_u8_tied2, svuint8_t,
		z0 = svsri_n_u8 (z1, z0, 2),
		z0 = svsri (z1, z0, 2))

/*
** sri_2_u8_untied:
**	mov	z0\.d, z1\.d
**	sri	z0\.b, z2\.b, #2
**	ret
*/
TEST_UNIFORM_Z (sri_2_u8_untied, svuint8_t,
		z0 = svsri_n_u8 (z1, z2, 2),
		z0 = svsri (z1, z2, 2))

/*
** sri_8_u8_tied1:
**	sri	z0\.b, z1\.b, #8
**	ret
*/
TEST_UNIFORM_Z (sri_8_u8_tied1, svuint8_t,
		z0 = svsri_n_u8 (z0, z1, 8),
		z0 = svsri (z0, z1, 8))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_8_u8_tied2, svuint8_t,
		z0 = svsri_n_u8 (z1, z0, 8),
		z0 = svsri (z1, z0, 8))

/*
** sri_8_u8_untied:
**	mov	z0\.d, z1\.d
**	sri	z0\.b, z2\.b, #8
**	ret
*/
TEST_UNIFORM_Z (sri_8_u8_untied, svuint8_t,
		z0 = svsri_n_u8 (z1, z2, 8),
		z0 = svsri (z1, z2, 8))
