/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rsubhnt_s16_tied1:
**	rsubhnt	z0\.b, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (rsubhnt_s16_tied1, svint8_t, svint16_t,
	     z0 = svrsubhnt_s16 (z0, z4, z5),
	     z0 = svrsubhnt (z0, z4, z5))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (rsubhnt_s16_tied2, svint8_t, svint16_t,
		 z0_res = svrsubhnt_s16 (z4, z0, z1),
		 z0_res = svrsubhnt (z4, z0, z1))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (rsubhnt_s16_tied3, svint8_t, svint16_t,
		 z0_res = svrsubhnt_s16 (z4, z1, z0),
		 z0_res = svrsubhnt (z4, z1, z0))

/*
** rsubhnt_s16_untied:
** (
**	mov	z0\.d, z1\.d
**	rsubhnt	z0\.b, z4\.h, z5\.h
** |
**	rsubhnt	z1\.b, z4\.h, z5\.h
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (rsubhnt_s16_untied, svint8_t, svint16_t,
	     z0 = svrsubhnt_s16 (z1, z4, z5),
	     z0 = svrsubhnt (z1, z4, z5))

/*
** rsubhnt_w0_s16_tied1:
**	mov	(z[0-9]+\.h), w0
**	rsubhnt	z0\.b, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (rsubhnt_w0_s16_tied1, svint8_t, svint16_t, int16_t,
	      z0 = svrsubhnt_n_s16 (z0, z4, x0),
	      z0 = svrsubhnt (z0, z4, x0))

/*
** rsubhnt_w0_s16_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	mov	z0\.d, z1\.d
**	rsubhnt	z0\.b, z4\.h, \1
** |
**	rsubhnt	z1\.b, z4\.h, \1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_ZX (rsubhnt_w0_s16_untied, svint8_t, svint16_t, int16_t,
	      z0 = svrsubhnt_n_s16 (z1, z4, x0),
	      z0 = svrsubhnt (z1, z4, x0))

/*
** rsubhnt_11_s16_tied1:
**	mov	(z[0-9]+\.h), #11
**	rsubhnt	z0\.b, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (rsubhnt_11_s16_tied1, svint8_t, svint16_t,
	     z0 = svrsubhnt_n_s16 (z0, z4, 11),
	     z0 = svrsubhnt (z0, z4, 11))

/*
** rsubhnt_11_s16_untied:
**	mov	(z[0-9]+\.h), #11
** (
**	mov	z0\.d, z1\.d
**	rsubhnt	z0\.b, z4\.h, \1
** |
**	rsubhnt	z1\.b, z4\.h, \1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (rsubhnt_11_s16_untied, svint8_t, svint16_t,
	     z0 = svrsubhnt_n_s16 (z1, z4, 11),
	     z0 = svrsubhnt (z1, z4, 11))
