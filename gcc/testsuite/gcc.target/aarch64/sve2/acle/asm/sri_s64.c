/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sri_1_s64_tied1:
**	sri	z0\.d, z1\.d, #1
**	ret
*/
TEST_UNIFORM_Z (sri_1_s64_tied1, svint64_t,
		z0 = svsri_n_s64 (z0, z1, 1),
		z0 = svsri (z0, z1, 1))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_1_s64_tied2, svint64_t,
		z0 = svsri_n_s64 (z1, z0, 1),
		z0 = svsri (z1, z0, 1))

/*
** sri_1_s64_untied:
**	mov	z0\.d, z1\.d
**	sri	z0\.d, z2\.d, #1
**	ret
*/
TEST_UNIFORM_Z (sri_1_s64_untied, svint64_t,
		z0 = svsri_n_s64 (z1, z2, 1),
		z0 = svsri (z1, z2, 1))

/*
** sri_2_s64_tied1:
**	sri	z0\.d, z1\.d, #2
**	ret
*/
TEST_UNIFORM_Z (sri_2_s64_tied1, svint64_t,
		z0 = svsri_n_s64 (z0, z1, 2),
		z0 = svsri (z0, z1, 2))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_2_s64_tied2, svint64_t,
		z0 = svsri_n_s64 (z1, z0, 2),
		z0 = svsri (z1, z0, 2))

/*
** sri_2_s64_untied:
**	mov	z0\.d, z1\.d
**	sri	z0\.d, z2\.d, #2
**	ret
*/
TEST_UNIFORM_Z (sri_2_s64_untied, svint64_t,
		z0 = svsri_n_s64 (z1, z2, 2),
		z0 = svsri (z1, z2, 2))

/*
** sri_64_s64_tied1:
**	sri	z0\.d, z1\.d, #64
**	ret
*/
TEST_UNIFORM_Z (sri_64_s64_tied1, svint64_t,
		z0 = svsri_n_s64 (z0, z1, 64),
		z0 = svsri (z0, z1, 64))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_64_s64_tied2, svint64_t,
		z0 = svsri_n_s64 (z1, z0, 64),
		z0 = svsri (z1, z0, 64))

/*
** sri_64_s64_untied:
**	mov	z0\.d, z1\.d
**	sri	z0\.d, z2\.d, #64
**	ret
*/
TEST_UNIFORM_Z (sri_64_s64_untied, svint64_t,
		z0 = svsri_n_s64 (z1, z2, 64),
		z0 = svsri (z1, z2, 64))
