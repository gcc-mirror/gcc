/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subhnt_u16_tied1:
**	subhnt	z0\.b, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (subhnt_u16_tied1, svuint8_t, svuint16_t,
	     z0 = svsubhnt_u16 (z0, z4, z5),
	     z0 = svsubhnt (z0, z4, z5))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (subhnt_u16_tied2, svuint8_t, svuint16_t,
		 z0_res = svsubhnt_u16 (z4, z0, z1),
		 z0_res = svsubhnt (z4, z0, z1))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (subhnt_u16_tied3, svuint8_t, svuint16_t,
		 z0_res = svsubhnt_u16 (z4, z1, z0),
		 z0_res = svsubhnt (z4, z1, z0))

/*
** subhnt_u16_untied:
** (
**	mov	z0\.d, z1\.d
**	subhnt	z0\.b, z4\.h, z5\.h
** |
**	subhnt	z1\.b, z4\.h, z5\.h
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (subhnt_u16_untied, svuint8_t, svuint16_t,
	     z0 = svsubhnt_u16 (z1, z4, z5),
	     z0 = svsubhnt (z1, z4, z5))

/*
** subhnt_w0_u16_tied1:
**	mov	(z[0-9]+\.h), w0
**	subhnt	z0\.b, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (subhnt_w0_u16_tied1, svuint8_t, svuint16_t, uint16_t,
	      z0 = svsubhnt_n_u16 (z0, z4, x0),
	      z0 = svsubhnt (z0, z4, x0))

/*
** subhnt_w0_u16_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	mov	z0\.d, z1\.d
**	subhnt	z0\.b, z4\.h, \1
** |
**	subhnt	z1\.b, z4\.h, \1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_ZX (subhnt_w0_u16_untied, svuint8_t, svuint16_t, uint16_t,
	      z0 = svsubhnt_n_u16 (z1, z4, x0),
	      z0 = svsubhnt (z1, z4, x0))

/*
** subhnt_11_u16_tied1:
**	mov	(z[0-9]+\.h), #11
**	subhnt	z0\.b, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (subhnt_11_u16_tied1, svuint8_t, svuint16_t,
	     z0 = svsubhnt_n_u16 (z0, z4, 11),
	     z0 = svsubhnt (z0, z4, 11))

/*
** subhnt_11_u16_untied:
**	mov	(z[0-9]+\.h), #11
** (
**	mov	z0\.d, z1\.d
**	subhnt	z0\.b, z4\.h, \1
** |
**	subhnt	z1\.b, z4\.h, \1
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (subhnt_11_u16_untied, svuint8_t, svuint16_t,
	     z0 = svsubhnt_n_u16 (z1, z4, 11),
	     z0 = svsubhnt (z1, z4, 11))
