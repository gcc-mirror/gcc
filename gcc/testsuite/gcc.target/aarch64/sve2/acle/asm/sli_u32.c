/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sli_0_u32_tied1:
**	sli	z0\.s, z1\.s, #0
**	ret
*/
TEST_UNIFORM_Z (sli_0_u32_tied1, svuint32_t,
		z0 = svsli_n_u32 (z0, z1, 0),
		z0 = svsli (z0, z1, 0))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sli_0_u32_tied2, svuint32_t,
		z0 = svsli_n_u32 (z1, z0, 0),
		z0 = svsli (z1, z0, 0))

/*
** sli_0_u32_untied:
**	mov	z0\.d, z1\.d
**	sli	z0\.s, z2\.s, #0
**	ret
*/
TEST_UNIFORM_Z (sli_0_u32_untied, svuint32_t,
		z0 = svsli_n_u32 (z1, z2, 0),
		z0 = svsli (z1, z2, 0))

/*
** sli_1_u32_tied1:
**	sli	z0\.s, z1\.s, #1
**	ret
*/
TEST_UNIFORM_Z (sli_1_u32_tied1, svuint32_t,
		z0 = svsli_n_u32 (z0, z1, 1),
		z0 = svsli (z0, z1, 1))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sli_1_u32_tied2, svuint32_t,
		z0 = svsli_n_u32 (z1, z0, 1),
		z0 = svsli (z1, z0, 1))

/*
** sli_1_u32_untied:
**	mov	z0\.d, z1\.d
**	sli	z0\.s, z2\.s, #1
**	ret
*/
TEST_UNIFORM_Z (sli_1_u32_untied, svuint32_t,
		z0 = svsli_n_u32 (z1, z2, 1),
		z0 = svsli (z1, z2, 1))

/*
** sli_31_u32_tied1:
**	sli	z0\.s, z1\.s, #31
**	ret
*/
TEST_UNIFORM_Z (sli_31_u32_tied1, svuint32_t,
		z0 = svsli_n_u32 (z0, z1, 31),
		z0 = svsli (z0, z1, 31))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sli_31_u32_tied2, svuint32_t,
		z0 = svsli_n_u32 (z1, z0, 31),
		z0 = svsli (z1, z0, 31))

/*
** sli_31_u32_untied:
**	mov	z0\.d, z1\.d
**	sli	z0\.s, z2\.s, #31
**	ret
*/
TEST_UNIFORM_Z (sli_31_u32_untied, svuint32_t,
		z0 = svsli_n_u32 (z1, z2, 31),
		z0 = svsli (z1, z2, 31))
