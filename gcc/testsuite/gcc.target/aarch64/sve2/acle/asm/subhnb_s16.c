/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subhnb_s16_tied1:
**	subhnb	z0\.b, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (subhnb_s16_tied1, svint8_t, svint16_t,
		    z0_res = svsubhnb_s16 (z0, z1),
		    z0_res = svsubhnb (z0, z1))

/*
** subhnb_s16_tied2:
**	subhnb	z0\.b, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (subhnb_s16_tied2, svint8_t, svint16_t,
		    z0_res = svsubhnb_s16 (z1, z0),
		    z0_res = svsubhnb (z1, z0))

/*
** subhnb_s16_untied:
**	subhnb	z0\.b, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (subhnb_s16_untied, svint8_t, svint16_t,
		    z0_res = svsubhnb_s16 (z1, z2),
		    z0_res = svsubhnb (z1, z2))

/*
** subhnb_w0_s16_tied1:
**	mov	(z[0-9]+\.h), w0
**	subhnb	z0\.b, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (subhnb_w0_s16_tied1, svint8_t, svint16_t, int16_t,
		     z0_res = svsubhnb_n_s16 (z0, x0),
		     z0_res = svsubhnb (z0, x0))

/*
** subhnb_w0_s16_untied:
**	mov	(z[0-9]+\.h), w0
**	subhnb	z0\.b, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (subhnb_w0_s16_untied, svint8_t, svint16_t, int16_t,
		     z0_res = svsubhnb_n_s16 (z1, x0),
		     z0_res = svsubhnb (z1, x0))

/*
** subhnb_11_s16_tied1:
**	mov	(z[0-9]+\.h), #11
**	subhnb	z0\.b, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (subhnb_11_s16_tied1, svint8_t, svint16_t,
		    z0_res = svsubhnb_n_s16 (z0, 11),
		    z0_res = svsubhnb (z0, 11))

/*
** subhnb_11_s16_untied:
**	mov	(z[0-9]+\.h), #11
**	subhnb	z0\.b, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (subhnb_11_s16_untied, svint8_t, svint16_t,
		    z0_res = svsubhnb_n_s16 (z1, 11),
		    z0_res = svsubhnb (z1, 11))
