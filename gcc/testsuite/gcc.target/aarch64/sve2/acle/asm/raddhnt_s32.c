/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** raddhnt_s32_tied1:
**	raddhnt	z0\.h, (z4\.s, z5\.s|z5\.s, z4\.s)
**	ret
*/
TEST_DUAL_Z (raddhnt_s32_tied1, svint16_t, svint32_t,
	     z0 = svraddhnt_s32 (z0, z4, z5),
	     z0 = svraddhnt (z0, z4, z5))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (raddhnt_s32_tied2, svint16_t, svint32_t,
		 z0_res = svraddhnt_s32 (z4, z0, z1),
		 z0_res = svraddhnt (z4, z0, z1))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (raddhnt_s32_tied3, svint16_t, svint32_t,
		 z0_res = svraddhnt_s32 (z4, z1, z0),
		 z0_res = svraddhnt (z4, z1, z0))

/*
** raddhnt_s32_untied:
** (
**	mov	z0\.d, z1\.d
**	raddhnt	z0\.h, (z4\.s, z5\.s|z5\.s, z4\.s)
** |
**	raddhnt	z1\.h, (z4\.s, z5\.s|z5\.s, z4\.s)
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (raddhnt_s32_untied, svint16_t, svint32_t,
	     z0 = svraddhnt_s32 (z1, z4, z5),
	     z0 = svraddhnt (z1, z4, z5))

/*
** raddhnt_w0_s32_tied1:
**	mov	(z[0-9]+\.s), w0
**	raddhnt	z0\.h, (z4\.s, \1|\1, z4\.s)
**	ret
*/
TEST_DUAL_ZX (raddhnt_w0_s32_tied1, svint16_t, svint32_t, int32_t,
	      z0 = svraddhnt_n_s32 (z0, z4, x0),
	      z0 = svraddhnt (z0, z4, x0))

/*
** raddhnt_w0_s32_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	mov	z0\.d, z1\.d
**	raddhnt	z0\.h, (z4\.s, \1|\1, z4\.s)
** |
**	raddhnt	z1\.h, (z4\.s, \1|\1, z4\.s)
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_ZX (raddhnt_w0_s32_untied, svint16_t, svint32_t, int32_t,
	      z0 = svraddhnt_n_s32 (z1, z4, x0),
	      z0 = svraddhnt (z1, z4, x0))

/*
** raddhnt_11_s32_tied1:
**	mov	(z[0-9]+\.s), #11
**	raddhnt	z0\.h, (z4\.s, \1|\1, z4\.s)
**	ret
*/
TEST_DUAL_Z (raddhnt_11_s32_tied1, svint16_t, svint32_t,
	     z0 = svraddhnt_n_s32 (z0, z4, 11),
	     z0 = svraddhnt (z0, z4, 11))

/*
** raddhnt_11_s32_untied:
**	mov	(z[0-9]+\.s), #11
** (
**	mov	z0\.d, z1\.d
**	raddhnt	z0\.h, (z4\.s, \1|\1, z4\.s)
** |
**	raddhnt	z1\.h, (z4\.s, \1|\1, z4\.s)
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (raddhnt_11_s32_untied, svint16_t, svint32_t,
	     z0 = svraddhnt_n_s32 (z1, z4, 11),
	     z0 = svraddhnt (z1, z4, 11))
