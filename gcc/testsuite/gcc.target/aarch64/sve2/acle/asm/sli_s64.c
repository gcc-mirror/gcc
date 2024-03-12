/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sli_0_s64_tied1:
**	sli	z0\.d, z1\.d, #0
**	ret
*/
TEST_UNIFORM_Z (sli_0_s64_tied1, svint64_t,
		z0 = svsli_n_s64 (z0, z1, 0),
		z0 = svsli (z0, z1, 0))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sli_0_s64_tied2, svint64_t,
		z0 = svsli_n_s64 (z1, z0, 0),
		z0 = svsli (z1, z0, 0))

/*
** sli_0_s64_untied:
** (
**	mov	z0\.d, z1\.d
**	sli	z0\.d, z2\.d, #0
** |
**	sli	z1\.d, z2\.d, #0
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sli_0_s64_untied, svint64_t,
		z0 = svsli_n_s64 (z1, z2, 0),
		z0 = svsli (z1, z2, 0))

/*
** sli_1_s64_tied1:
**	sli	z0\.d, z1\.d, #1
**	ret
*/
TEST_UNIFORM_Z (sli_1_s64_tied1, svint64_t,
		z0 = svsli_n_s64 (z0, z1, 1),
		z0 = svsli (z0, z1, 1))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sli_1_s64_tied2, svint64_t,
		z0 = svsli_n_s64 (z1, z0, 1),
		z0 = svsli (z1, z0, 1))

/*
** sli_1_s64_untied:
** (
**	mov	z0\.d, z1\.d
**	sli	z0\.d, z2\.d, #1
** |
**	sli	z1\.d, z2\.d, #1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sli_1_s64_untied, svint64_t,
		z0 = svsli_n_s64 (z1, z2, 1),
		z0 = svsli (z1, z2, 1))

/*
** sli_63_s64_tied1:
**	sli	z0\.d, z1\.d, #63
**	ret
*/
TEST_UNIFORM_Z (sli_63_s64_tied1, svint64_t,
		z0 = svsli_n_s64 (z0, z1, 63),
		z0 = svsli (z0, z1, 63))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_Z (sli_63_s64_tied2, svint64_t,
		z0 = svsli_n_s64 (z1, z0, 63),
		z0 = svsli (z1, z0, 63))

/*
** sli_63_s64_untied:
** (
**	mov	z0\.d, z1\.d
**	sli	z0\.d, z2\.d, #63
** |
**	sli	z1\.d, z2\.d, #63
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (sli_63_s64_untied, svint64_t,
		z0 = svsli_n_s64 (z1, z2, 63),
		z0 = svsli (z1, z2, 63))
