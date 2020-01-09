/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sli_0_s16_tied1:
**	sli	z0\.h, z1\.h, #0
**	ret
*/
TEST_UNIFORM_Z (sli_0_s16_tied1, svint16_t,
		z0 = svsli_n_s16 (z0, z1, 0),
		z0 = svsli (z0, z1, 0))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sli_0_s16_tied2, svint16_t,
		z0 = svsli_n_s16 (z1, z0, 0),
		z0 = svsli (z1, z0, 0))

/*
** sli_0_s16_untied:
**	mov	z0\.d, z1\.d
**	sli	z0\.h, z2\.h, #0
**	ret
*/
TEST_UNIFORM_Z (sli_0_s16_untied, svint16_t,
		z0 = svsli_n_s16 (z1, z2, 0),
		z0 = svsli (z1, z2, 0))

/*
** sli_1_s16_tied1:
**	sli	z0\.h, z1\.h, #1
**	ret
*/
TEST_UNIFORM_Z (sli_1_s16_tied1, svint16_t,
		z0 = svsli_n_s16 (z0, z1, 1),
		z0 = svsli (z0, z1, 1))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sli_1_s16_tied2, svint16_t,
		z0 = svsli_n_s16 (z1, z0, 1),
		z0 = svsli (z1, z0, 1))

/*
** sli_1_s16_untied:
**	mov	z0\.d, z1\.d
**	sli	z0\.h, z2\.h, #1
**	ret
*/
TEST_UNIFORM_Z (sli_1_s16_untied, svint16_t,
		z0 = svsli_n_s16 (z1, z2, 1),
		z0 = svsli (z1, z2, 1))

/*
** sli_15_s16_tied1:
**	sli	z0\.h, z1\.h, #15
**	ret
*/
TEST_UNIFORM_Z (sli_15_s16_tied1, svint16_t,
		z0 = svsli_n_s16 (z0, z1, 15),
		z0 = svsli (z0, z1, 15))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sli_15_s16_tied2, svint16_t,
		z0 = svsli_n_s16 (z1, z0, 15),
		z0 = svsli (z1, z0, 15))

/*
** sli_15_s16_untied:
**	mov	z0\.d, z1\.d
**	sli	z0\.h, z2\.h, #15
**	ret
*/
TEST_UNIFORM_Z (sli_15_s16_untied, svint16_t,
		z0 = svsli_n_s16 (z1, z2, 15),
		z0 = svsli (z1, z2, 15))
