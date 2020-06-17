/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** raddhnt_s16_tied1:
**	raddhnt	z0\.b, (z4\.h, z5\.h|z5\.h, z4\.h)
**	ret
*/
TEST_DUAL_Z (raddhnt_s16_tied1, svint8_t, svint16_t,
	     z0 = svraddhnt_s16 (z0, z4, z5),
	     z0 = svraddhnt (z0, z4, z5))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (raddhnt_s16_tied2, svint8_t, svint16_t,
		 z0_res = svraddhnt_s16 (z4, z0, z1),
		 z0_res = svraddhnt (z4, z0, z1))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (raddhnt_s16_tied3, svint8_t, svint16_t,
		 z0_res = svraddhnt_s16 (z4, z1, z0),
		 z0_res = svraddhnt (z4, z1, z0))

/*
** raddhnt_s16_untied:
** (
**	mov	z0\.d, z1\.d
**	raddhnt	z0\.b, (z4\.h, z5\.h|z5\.h, z4\.h)
** |
**	raddhnt	z1\.b, (z4\.h, z5\.h|z5\.h, z4\.h)
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (raddhnt_s16_untied, svint8_t, svint16_t,
	     z0 = svraddhnt_s16 (z1, z4, z5),
	     z0 = svraddhnt (z1, z4, z5))

/*
** raddhnt_w0_s16_tied1:
**	mov	(z[0-9]+\.h), w0
**	raddhnt	z0\.b, (z4\.h, \1|\1, z4\.h)
**	ret
*/
TEST_DUAL_ZX (raddhnt_w0_s16_tied1, svint8_t, svint16_t, int16_t,
	      z0 = svraddhnt_n_s16 (z0, z4, x0),
	      z0 = svraddhnt (z0, z4, x0))

/*
** raddhnt_w0_s16_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	mov	z0\.d, z1\.d
**	raddhnt	z0\.b, (z4\.h, \1|\1, z4\.h)
** |
**	raddhnt	z1\.b, (z4\.h, \1|\1, z4\.h)
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_ZX (raddhnt_w0_s16_untied, svint8_t, svint16_t, int16_t,
	      z0 = svraddhnt_n_s16 (z1, z4, x0),
	      z0 = svraddhnt (z1, z4, x0))

/*
** raddhnt_11_s16_tied1:
**	mov	(z[0-9]+\.h), #11
**	raddhnt	z0\.b, (z4\.h, \1|\1, z4\.h)
**	ret
*/
TEST_DUAL_Z (raddhnt_11_s16_tied1, svint8_t, svint16_t,
	     z0 = svraddhnt_n_s16 (z0, z4, 11),
	     z0 = svraddhnt (z0, z4, 11))

/*
** raddhnt_11_s16_untied:
**	mov	(z[0-9]+\.h), #11
** (
**	mov	z0\.d, z1\.d
**	raddhnt	z0\.b, (z4\.h, \1|\1, z4\.h)
** |
**	raddhnt	z1\.b, (z4\.h, \1|\1, z4\.h)
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (raddhnt_11_s16_untied, svint8_t, svint16_t,
	     z0 = svraddhnt_n_s16 (z1, z4, 11),
	     z0 = svraddhnt (z1, z4, 11))
