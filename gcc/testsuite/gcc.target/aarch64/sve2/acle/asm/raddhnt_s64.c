/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** raddhnt_s64_tied1:
**	raddhnt	z0\.s, (z4\.d, z5\.d|z5\.d, z4\.d)
**	ret
*/
TEST_DUAL_Z (raddhnt_s64_tied1, svint32_t, svint64_t,
	     z0 = svraddhnt_s64 (z0, z4, z5),
	     z0 = svraddhnt (z0, z4, z5))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (raddhnt_s64_tied2, svint32_t, svint64_t,
		 z0_res = svraddhnt_s64 (z4, z0, z1),
		 z0_res = svraddhnt (z4, z0, z1))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (raddhnt_s64_tied3, svint32_t, svint64_t,
		 z0_res = svraddhnt_s64 (z4, z1, z0),
		 z0_res = svraddhnt (z4, z1, z0))

/*
** raddhnt_s64_untied:
** (
**	mov	z0\.d, z1\.d
**	raddhnt	z0\.s, (z4\.d, z5\.d|z5\.d, z4\.d)
** |
**	raddhnt	z1\.s, (z4\.d, z5\.d|z5\.d, z4\.d)
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (raddhnt_s64_untied, svint32_t, svint64_t,
	     z0 = svraddhnt_s64 (z1, z4, z5),
	     z0 = svraddhnt (z1, z4, z5))

/*
** raddhnt_x0_s64_tied1:
**	mov	(z[0-9]+\.d), x0
**	raddhnt	z0\.s, (z4\.d, \1|\1, z4\.d)
**	ret
*/
TEST_DUAL_ZX (raddhnt_x0_s64_tied1, svint32_t, svint64_t, int64_t,
	      z0 = svraddhnt_n_s64 (z0, z4, x0),
	      z0 = svraddhnt (z0, z4, x0))

/*
** raddhnt_x0_s64_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	mov	z0\.d, z1\.d
**	raddhnt	z0\.s, (z4\.d, \1|\1, z4\.d)
** |
**	raddhnt	z1\.s, (z4\.d, \1|\1, z4\.d)
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_ZX (raddhnt_x0_s64_untied, svint32_t, svint64_t, int64_t,
	      z0 = svraddhnt_n_s64 (z1, z4, x0),
	      z0 = svraddhnt (z1, z4, x0))

/*
** raddhnt_11_s64_tied1:
**	mov	(z[0-9]+\.d), #11
**	raddhnt	z0\.s, (z4\.d, \1|\1, z4\.d)
**	ret
*/
TEST_DUAL_Z (raddhnt_11_s64_tied1, svint32_t, svint64_t,
	     z0 = svraddhnt_n_s64 (z0, z4, 11),
	     z0 = svraddhnt (z0, z4, 11))

/*
** raddhnt_11_s64_untied:
**	mov	(z[0-9]+\.d), #11
** (
**	mov	z0\.d, z1\.d
**	raddhnt	z0\.s, (z4\.d, \1|\1, z4\.d)
** |
**	raddhnt	z1\.s, (z4\.d, \1|\1, z4\.d)
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (raddhnt_11_s64_untied, svint32_t, svint64_t,
	     z0 = svraddhnt_n_s64 (z1, z4, 11),
	     z0 = svraddhnt (z1, z4, 11))
