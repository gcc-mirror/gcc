/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addhnt_s64_tied1:
**	addhnt	z0\.s, (z4\.d, z5\.d|z5\.d, z4\.d)
**	ret
*/
TEST_DUAL_Z (addhnt_s64_tied1, svint32_t, svint64_t,
	     z0 = svaddhnt_s64 (z0, z4, z5),
	     z0 = svaddhnt (z0, z4, z5))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (addhnt_s64_tied2, svint32_t, svint64_t,
		 z0_res = svaddhnt_s64 (z4, z0, z1),
		 z0_res = svaddhnt (z4, z0, z1))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (addhnt_s64_tied3, svint32_t, svint64_t,
		 z0_res = svaddhnt_s64 (z4, z1, z0),
		 z0_res = svaddhnt (z4, z1, z0))

/*
** addhnt_s64_untied:
** (
**	mov	z0\.d, z1\.d
**	addhnt	z0\.s, (z4\.d, z5\.d|z5\.d, z4\.d)
** |
**	addhnt	z1\.s, (z4\.d, z5\.d|z5\.d, z4\.d)
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (addhnt_s64_untied, svint32_t, svint64_t,
	     z0 = svaddhnt_s64 (z1, z4, z5),
	     z0 = svaddhnt (z1, z4, z5))

/*
** addhnt_x0_s64_tied1:
**	mov	(z[0-9]+\.d), x0
**	addhnt	z0\.s, (z4\.d, \1|\1, z4\.d)
**	ret
*/
TEST_DUAL_ZX (addhnt_x0_s64_tied1, svint32_t, svint64_t, int64_t,
	      z0 = svaddhnt_n_s64 (z0, z4, x0),
	      z0 = svaddhnt (z0, z4, x0))

/*
** addhnt_x0_s64_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	mov	z0\.d, z1\.d
**	addhnt	z0\.s, (z4\.d, \1|\1, z4\.d)
** |
**	addhnt	z1\.s, (z4\.d, \1|\1, z4\.d)
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_ZX (addhnt_x0_s64_untied, svint32_t, svint64_t, int64_t,
	      z0 = svaddhnt_n_s64 (z1, z4, x0),
	      z0 = svaddhnt (z1, z4, x0))

/*
** addhnt_11_s64_tied1:
**	mov	(z[0-9]+\.d), #11
**	addhnt	z0\.s, (z4\.d, \1|\1, z4\.d)
**	ret
*/
TEST_DUAL_Z (addhnt_11_s64_tied1, svint32_t, svint64_t,
	     z0 = svaddhnt_n_s64 (z0, z4, 11),
	     z0 = svaddhnt (z0, z4, 11))

/*
** addhnt_11_s64_untied:
**	mov	(z[0-9]+\.d), #11
** (
**	mov	z0\.d, z1\.d
**	addhnt	z0\.s, (z4\.d, \1|\1, z4\.d)
** |
**	addhnt	z1\.s, (z4\.d, \1|\1, z4\.d)
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (addhnt_11_s64_untied, svint32_t, svint64_t,
	     z0 = svaddhnt_n_s64 (z1, z4, 11),
	     z0 = svaddhnt (z1, z4, 11))
