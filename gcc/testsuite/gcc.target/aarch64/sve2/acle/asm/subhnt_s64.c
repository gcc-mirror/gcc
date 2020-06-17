/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subhnt_s64_tied1:
**	subhnt	z0\.s, z4\.d, z5\.d
**	ret
*/
TEST_DUAL_Z (subhnt_s64_tied1, svint32_t, svint64_t,
	     z0 = svsubhnt_s64 (z0, z4, z5),
	     z0 = svsubhnt (z0, z4, z5))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (subhnt_s64_tied2, svint32_t, svint64_t,
		 z0_res = svsubhnt_s64 (z4, z0, z1),
		 z0_res = svsubhnt (z4, z0, z1))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (subhnt_s64_tied3, svint32_t, svint64_t,
		 z0_res = svsubhnt_s64 (z4, z1, z0),
		 z0_res = svsubhnt (z4, z1, z0))

/*
** subhnt_s64_untied:
** (
**	mov	z0\.d, z1\.d
**	subhnt	z0\.s, z4\.d, z5\.d
** |
**	subhnt	z1\.s, z4\.d, z5\.d
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (subhnt_s64_untied, svint32_t, svint64_t,
	     z0 = svsubhnt_s64 (z1, z4, z5),
	     z0 = svsubhnt (z1, z4, z5))

/*
** subhnt_x0_s64_tied1:
**	mov	(z[0-9]+\.d), x0
**	subhnt	z0\.s, z4\.d, \1
**	ret
*/
TEST_DUAL_ZX (subhnt_x0_s64_tied1, svint32_t, svint64_t, int64_t,
	      z0 = svsubhnt_n_s64 (z0, z4, x0),
	      z0 = svsubhnt (z0, z4, x0))

/*
** subhnt_x0_s64_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	mov	z0\.d, z1\.d
**	subhnt	z0\.s, z4\.d, \1
** |
**	subhnt	z1\.s, z4\.d, \1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_ZX (subhnt_x0_s64_untied, svint32_t, svint64_t, int64_t,
	      z0 = svsubhnt_n_s64 (z1, z4, x0),
	      z0 = svsubhnt (z1, z4, x0))

/*
** subhnt_11_s64_tied1:
**	mov	(z[0-9]+\.d), #11
**	subhnt	z0\.s, z4\.d, \1
**	ret
*/
TEST_DUAL_Z (subhnt_11_s64_tied1, svint32_t, svint64_t,
	     z0 = svsubhnt_n_s64 (z0, z4, 11),
	     z0 = svsubhnt (z0, z4, 11))

/*
** subhnt_11_s64_untied:
**	mov	(z[0-9]+\.d), #11
** (
**	mov	z0\.d, z1\.d
**	subhnt	z0\.s, z4\.d, \1
** |
**	subhnt	z1\.s, z4\.d, \1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (subhnt_11_s64_untied, svint32_t, svint64_t,
	     z0 = svsubhnt_n_s64 (z1, z4, 11),
	     z0 = svsubhnt (z1, z4, 11))
