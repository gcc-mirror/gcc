/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sli_0_u8_tied1:
**	sli	z0\.b, z1\.b, #0
**	ret
*/
TEST_UNIFORM_Z (sli_0_u8_tied1, svuint8_t,
		z0 = svsli_n_u8 (z0, z1, 0),
		z0 = svsli (z0, z1, 0))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sli_0_u8_tied2, svuint8_t,
		z0 = svsli_n_u8 (z1, z0, 0),
		z0 = svsli (z1, z0, 0))

/*
** sli_0_u8_untied:
** (
**	mov	z0\.d, z1\.d
**	sli	z0\.b, z2\.b, #0
** |
**	sli	z1\.b, z2\.b, #0
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sli_0_u8_untied, svuint8_t,
		z0 = svsli_n_u8 (z1, z2, 0),
		z0 = svsli (z1, z2, 0))

/*
** sli_1_u8_tied1:
**	sli	z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (sli_1_u8_tied1, svuint8_t,
		z0 = svsli_n_u8 (z0, z1, 1),
		z0 = svsli (z0, z1, 1))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sli_1_u8_tied2, svuint8_t,
		z0 = svsli_n_u8 (z1, z0, 1),
		z0 = svsli (z1, z0, 1))

/*
** sli_1_u8_untied:
** (
**	mov	z0\.d, z1\.d
**	sli	z0\.b, z2\.b, #1
** |
**	sli	z1\.b, z2\.b, #1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sli_1_u8_untied, svuint8_t,
		z0 = svsli_n_u8 (z1, z2, 1),
		z0 = svsli (z1, z2, 1))

/*
** sli_7_u8_tied1:
**	sli	z0\.b, z1\.b, #7
**	ret
*/
TEST_UNIFORM_Z (sli_7_u8_tied1, svuint8_t,
		z0 = svsli_n_u8 (z0, z1, 7),
		z0 = svsli (z0, z1, 7))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sli_7_u8_tied2, svuint8_t,
		z0 = svsli_n_u8 (z1, z0, 7),
		z0 = svsli (z1, z0, 7))

/*
** sli_7_u8_untied:
** (
**	mov	z0\.d, z1\.d
**	sli	z0\.b, z2\.b, #7
** |
**	sli	z1\.b, z2\.b, #7
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sli_7_u8_untied, svuint8_t,
		z0 = svsli_n_u8 (z1, z2, 7),
		z0 = svsli (z1, z2, 7))
