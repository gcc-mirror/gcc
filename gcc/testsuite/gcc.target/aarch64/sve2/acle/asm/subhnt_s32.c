/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subhnt_s32_tied1:
**	subhnt	z0\.h, z4\.s, z5\.s
**	ret
*/
TEST_DUAL_Z (subhnt_s32_tied1, svint16_t, svint32_t,
	     z0 = svsubhnt_s32 (z0, z4, z5),
	     z0 = svsubhnt (z0, z4, z5))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (subhnt_s32_tied2, svint16_t, svint32_t,
		 z0_res = svsubhnt_s32 (z4, z0, z1),
		 z0_res = svsubhnt (z4, z0, z1))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (subhnt_s32_tied3, svint16_t, svint32_t,
		 z0_res = svsubhnt_s32 (z4, z1, z0),
		 z0_res = svsubhnt (z4, z1, z0))

/*
** subhnt_s32_untied:
** (
**	mov	z0\.d, z1\.d
**	subhnt	z0\.h, z4\.s, z5\.s
** |
**	subhnt	z1\.h, z4\.s, z5\.s
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (subhnt_s32_untied, svint16_t, svint32_t,
	     z0 = svsubhnt_s32 (z1, z4, z5),
	     z0 = svsubhnt (z1, z4, z5))

/*
** subhnt_w0_s32_tied1:
**	mov	(z[0-9]+\.s), w0
**	subhnt	z0\.h, z4\.s, \1
**	ret
*/
TEST_DUAL_ZX (subhnt_w0_s32_tied1, svint16_t, svint32_t, int32_t,
	      z0 = svsubhnt_n_s32 (z0, z4, x0),
	      z0 = svsubhnt (z0, z4, x0))

/*
** subhnt_w0_s32_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	mov	z0\.d, z1\.d
**	subhnt	z0\.h, z4\.s, \1
** |
**	subhnt	z1\.h, z4\.s, \1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_ZX (subhnt_w0_s32_untied, svint16_t, svint32_t, int32_t,
	      z0 = svsubhnt_n_s32 (z1, z4, x0),
	      z0 = svsubhnt (z1, z4, x0))

/*
** subhnt_11_s32_tied1:
**	mov	(z[0-9]+\.s), #11
**	subhnt	z0\.h, z4\.s, \1
**	ret
*/
TEST_DUAL_Z (subhnt_11_s32_tied1, svint16_t, svint32_t,
	     z0 = svsubhnt_n_s32 (z0, z4, 11),
	     z0 = svsubhnt (z0, z4, 11))

/*
** subhnt_11_s32_untied:
**	mov	(z[0-9]+\.s), #11
** (
**	mov	z0\.d, z1\.d
**	subhnt	z0\.h, z4\.s, \1
** |
**	subhnt	z1\.h, z4\.s, \1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (subhnt_11_s32_untied, svint16_t, svint32_t,
	     z0 = svsubhnt_n_s32 (z1, z4, 11),
	     z0 = svsubhnt (z1, z4, 11))
