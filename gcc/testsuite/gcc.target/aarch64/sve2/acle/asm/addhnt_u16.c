/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addhnt_u16_tied1:
**	addhnt	z0\.b, (z4\.h, z5\.h|z5\.h, z4\.h)
**	ret
*/
TEST_DUAL_Z (addhnt_u16_tied1, svuint8_t, svuint16_t,
	     z0 = svaddhnt_u16 (z0, z4, z5),
	     z0 = svaddhnt (z0, z4, z5))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (addhnt_u16_tied2, svuint8_t, svuint16_t,
		 z0_res = svaddhnt_u16 (z4, z0, z1),
		 z0_res = svaddhnt (z4, z0, z1))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (addhnt_u16_tied3, svuint8_t, svuint16_t,
		 z0_res = svaddhnt_u16 (z4, z1, z0),
		 z0_res = svaddhnt (z4, z1, z0))

/*
** addhnt_u16_untied:
** (
**	mov	z0\.d, z1\.d
**	addhnt	z0\.b, (z4\.h, z5\.h|z5\.h, z4\.h)
** |
**	addhnt	z1\.b, (z4\.h, z5\.h|z5\.h, z4\.h)
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (addhnt_u16_untied, svuint8_t, svuint16_t,
	     z0 = svaddhnt_u16 (z1, z4, z5),
	     z0 = svaddhnt (z1, z4, z5))

/*
** addhnt_w0_u16_tied1:
**	mov	(z[0-9]+\.h), w0
**	addhnt	z0\.b, (z4\.h, \1|\1, z4\.h)
**	ret
*/
TEST_DUAL_ZX (addhnt_w0_u16_tied1, svuint8_t, svuint16_t, uint16_t,
	      z0 = svaddhnt_n_u16 (z0, z4, x0),
	      z0 = svaddhnt (z0, z4, x0))

/*
** addhnt_w0_u16_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	mov	z0\.d, z1\.d
**	addhnt	z0\.b, (z4\.h, \1|\1, z4\.h)
** |
**	addhnt	z1\.b, (z4\.h, \1|\1, z4\.h)
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_ZX (addhnt_w0_u16_untied, svuint8_t, svuint16_t, uint16_t,
	      z0 = svaddhnt_n_u16 (z1, z4, x0),
	      z0 = svaddhnt (z1, z4, x0))

/*
** addhnt_11_u16_tied1:
**	mov	(z[0-9]+\.h), #11
**	addhnt	z0\.b, (z4\.h, \1|\1, z4\.h)
**	ret
*/
TEST_DUAL_Z (addhnt_11_u16_tied1, svuint8_t, svuint16_t,
	     z0 = svaddhnt_n_u16 (z0, z4, 11),
	     z0 = svaddhnt (z0, z4, 11))

/*
** addhnt_11_u16_untied:
**	mov	(z[0-9]+\.h), #11
** (
**	mov	z0\.d, z1\.d
**	addhnt	z0\.b, (z4\.h, \1|\1, z4\.h)
** |
**	addhnt	z1\.b, (z4\.h, \1|\1, z4\.h)
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (addhnt_11_u16_untied, svuint8_t, svuint16_t,
	     z0 = svaddhnt_n_u16 (z1, z4, 11),
	     z0 = svaddhnt (z1, z4, 11))
