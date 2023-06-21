/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sri_1_u64_tied1:
**	sri	z0\.d, z1\.d, #1
**	ret
*/
TEST_UNIFORM_Z (sri_1_u64_tied1, svuint64_t,
		z0 = svsri_n_u64 (z0, z1, 1),
		z0 = svsri (z0, z1, 1))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_1_u64_tied2, svuint64_t,
		z0 = svsri_n_u64 (z1, z0, 1),
		z0 = svsri (z1, z0, 1))

/*
** sri_1_u64_untied:
** (
**	mov	z0\.d, z1\.d
**	sri	z0\.d, z2\.d, #1
** |
**	sri	z1\.d, z2\.d, #1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sri_1_u64_untied, svuint64_t,
		z0 = svsri_n_u64 (z1, z2, 1),
		z0 = svsri (z1, z2, 1))

/*
** sri_2_u64_tied1:
**	sri	z0\.d, z1\.d, #2
**	ret
*/
TEST_UNIFORM_Z (sri_2_u64_tied1, svuint64_t,
		z0 = svsri_n_u64 (z0, z1, 2),
		z0 = svsri (z0, z1, 2))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_2_u64_tied2, svuint64_t,
		z0 = svsri_n_u64 (z1, z0, 2),
		z0 = svsri (z1, z0, 2))

/*
** sri_2_u64_untied:
** (
**	mov	z0\.d, z1\.d
**	sri	z0\.d, z2\.d, #2
** |
**	sri	z1\.d, z2\.d, #2
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sri_2_u64_untied, svuint64_t,
		z0 = svsri_n_u64 (z1, z2, 2),
		z0 = svsri (z1, z2, 2))

/*
** sri_64_u64_tied1:
**	sri	z0\.d, z1\.d, #64
**	ret
*/
TEST_UNIFORM_Z (sri_64_u64_tied1, svuint64_t,
		z0 = svsri_n_u64 (z0, z1, 64),
		z0 = svsri (z0, z1, 64))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_64_u64_tied2, svuint64_t,
		z0 = svsri_n_u64 (z1, z0, 64),
		z0 = svsri (z1, z0, 64))

/*
** sri_64_u64_untied:
** (
**	mov	z0\.d, z1\.d
**	sri	z0\.d, z2\.d, #64
** |
**	sri	z1\.d, z2\.d, #64
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sri_64_u64_untied, svuint64_t,
		z0 = svsri_n_u64 (z1, z2, 64),
		z0 = svsri (z1, z2, 64))
