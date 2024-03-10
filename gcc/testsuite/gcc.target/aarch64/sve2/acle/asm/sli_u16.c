/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sli_0_u16_tied1:
**	sli	z0\.h, z1\.h, #0
**	ret
*/
TEST_UNIFORM_Z (sli_0_u16_tied1, svuint16_t,
		z0 = svsli_n_u16 (z0, z1, 0),
		z0 = svsli (z0, z1, 0))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sli_0_u16_tied2, svuint16_t,
		z0 = svsli_n_u16 (z1, z0, 0),
		z0 = svsli (z1, z0, 0))

/*
** sli_0_u16_untied:
** (
**	mov	z0\.d, z1\.d
**	sli	z0\.h, z2\.h, #0
** |
**	sli	z1\.h, z2\.h, #0
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sli_0_u16_untied, svuint16_t,
		z0 = svsli_n_u16 (z1, z2, 0),
		z0 = svsli (z1, z2, 0))

/*
** sli_1_u16_tied1:
**	sli	z0\.h, z1\.h, #1
**	ret
*/
TEST_UNIFORM_Z (sli_1_u16_tied1, svuint16_t,
		z0 = svsli_n_u16 (z0, z1, 1),
		z0 = svsli (z0, z1, 1))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sli_1_u16_tied2, svuint16_t,
		z0 = svsli_n_u16 (z1, z0, 1),
		z0 = svsli (z1, z0, 1))

/*
** sli_1_u16_untied:
** (
**	mov	z0\.d, z1\.d
**	sli	z0\.h, z2\.h, #1
** |
**	sli	z1\.h, z2\.h, #1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sli_1_u16_untied, svuint16_t,
		z0 = svsli_n_u16 (z1, z2, 1),
		z0 = svsli (z1, z2, 1))

/*
** sli_15_u16_tied1:
**	sli	z0\.h, z1\.h, #15
**	ret
*/
TEST_UNIFORM_Z (sli_15_u16_tied1, svuint16_t,
		z0 = svsli_n_u16 (z0, z1, 15),
		z0 = svsli (z0, z1, 15))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sli_15_u16_tied2, svuint16_t,
		z0 = svsli_n_u16 (z1, z0, 15),
		z0 = svsli (z1, z0, 15))

/*
** sli_15_u16_untied:
** (
**	mov	z0\.d, z1\.d
**	sli	z0\.h, z2\.h, #15
** |
**	sli	z1\.h, z2\.h, #15
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sli_15_u16_untied, svuint16_t,
		z0 = svsli_n_u16 (z1, z2, 15),
		z0 = svsli (z1, z2, 15))
