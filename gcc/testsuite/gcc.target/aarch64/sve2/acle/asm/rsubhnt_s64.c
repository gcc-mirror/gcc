/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rsubhnt_s64_tied1:
**	rsubhnt	z0\.s, z4\.d, z5\.d
**	ret
*/
TEST_DUAL_Z (rsubhnt_s64_tied1, svint32_t, svint64_t,
	     z0 = svrsubhnt_s64 (z0, z4, z5),
	     z0 = svrsubhnt (z0, z4, z5))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (rsubhnt_s64_tied2, svint32_t, svint64_t,
		 z0_res = svrsubhnt_s64 (z4, z0, z1),
		 z0_res = svrsubhnt (z4, z0, z1))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (rsubhnt_s64_tied3, svint32_t, svint64_t,
		 z0_res = svrsubhnt_s64 (z4, z1, z0),
		 z0_res = svrsubhnt (z4, z1, z0))

/*
** rsubhnt_s64_untied:
** (
**	mov	z0\.d, z1\.d
**	rsubhnt	z0\.s, z4\.d, z5\.d
** |
**	rsubhnt	z1\.s, z4\.d, z5\.d
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (rsubhnt_s64_untied, svint32_t, svint64_t,
	     z0 = svrsubhnt_s64 (z1, z4, z5),
	     z0 = svrsubhnt (z1, z4, z5))

/*
** rsubhnt_x0_s64_tied1:
**	mov	(z[0-9]+\.d), x0
**	rsubhnt	z0\.s, z4\.d, \1
**	ret
*/
TEST_DUAL_ZX (rsubhnt_x0_s64_tied1, svint32_t, svint64_t, int64_t,
	      z0 = svrsubhnt_n_s64 (z0, z4, x0),
	      z0 = svrsubhnt (z0, z4, x0))

/*
** rsubhnt_x0_s64_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	mov	z0\.d, z1\.d
**	rsubhnt	z0\.s, z4\.d, \1
** |
**	rsubhnt	z1\.s, z4\.d, \1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_ZX (rsubhnt_x0_s64_untied, svint32_t, svint64_t, int64_t,
	      z0 = svrsubhnt_n_s64 (z1, z4, x0),
	      z0 = svrsubhnt (z1, z4, x0))

/*
** rsubhnt_11_s64_tied1:
**	mov	(z[0-9]+\.d), #11
**	rsubhnt	z0\.s, z4\.d, \1
**	ret
*/
TEST_DUAL_Z (rsubhnt_11_s64_tied1, svint32_t, svint64_t,
	     z0 = svrsubhnt_n_s64 (z0, z4, 11),
	     z0 = svrsubhnt (z0, z4, 11))

/*
** rsubhnt_11_s64_untied:
**	mov	(z[0-9]+\.d), #11
** (
**	mov	z0\.d, z1\.d
**	rsubhnt	z0\.s, z4\.d, \1
** |
**	rsubhnt	z1\.s, z4\.d, \1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (rsubhnt_11_s64_untied, svint32_t, svint64_t,
	     z0 = svrsubhnt_n_s64 (z1, z4, 11),
	     z0 = svrsubhnt (z1, z4, 11))
