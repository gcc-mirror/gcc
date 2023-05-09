/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sri_1_u32_tied1:
**	sri	z0\.s, z1\.s, #1
**	ret
*/
TEST_UNIFORM_Z (sri_1_u32_tied1, svuint32_t,
		z0 = svsri_n_u32 (z0, z1, 1),
		z0 = svsri (z0, z1, 1))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_1_u32_tied2, svuint32_t,
		z0 = svsri_n_u32 (z1, z0, 1),
		z0 = svsri (z1, z0, 1))

/*
** sri_1_u32_untied:
** (
**	mov	z0\.d, z1\.d
**	sri	z0\.s, z2\.s, #1
** |
**	sri	z1\.s, z2\.s, #1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sri_1_u32_untied, svuint32_t,
		z0 = svsri_n_u32 (z1, z2, 1),
		z0 = svsri (z1, z2, 1))

/*
** sri_2_u32_tied1:
**	sri	z0\.s, z1\.s, #2
**	ret
*/
TEST_UNIFORM_Z (sri_2_u32_tied1, svuint32_t,
		z0 = svsri_n_u32 (z0, z1, 2),
		z0 = svsri (z0, z1, 2))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_2_u32_tied2, svuint32_t,
		z0 = svsri_n_u32 (z1, z0, 2),
		z0 = svsri (z1, z0, 2))

/*
** sri_2_u32_untied:
** (
**	mov	z0\.d, z1\.d
**	sri	z0\.s, z2\.s, #2
** |
**	sri	z1\.s, z2\.s, #2
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sri_2_u32_untied, svuint32_t,
		z0 = svsri_n_u32 (z1, z2, 2),
		z0 = svsri (z1, z2, 2))

/*
** sri_32_u32_tied1:
**	sri	z0\.s, z1\.s, #32
**	ret
*/
TEST_UNIFORM_Z (sri_32_u32_tied1, svuint32_t,
		z0 = svsri_n_u32 (z0, z1, 32),
		z0 = svsri (z0, z1, 32))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sri_32_u32_tied2, svuint32_t,
		z0 = svsri_n_u32 (z1, z0, 32),
		z0 = svsri (z1, z0, 32))

/*
** sri_32_u32_untied:
** (
**	mov	z0\.d, z1\.d
**	sri	z0\.s, z2\.s, #32
** |
**	sri	z1\.s, z2\.s, #32
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sri_32_u32_untied, svuint32_t,
		z0 = svsri_n_u32 (z1, z2, 32),
		z0 = svsri (z1, z2, 32))
