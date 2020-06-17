/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rsubhnb_s16_tied1:
**	rsubhnb	z0\.b, z0\.h, z1\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (rsubhnb_s16_tied1, svint8_t, svint16_t,
		    z0_res = svrsubhnb_s16 (z0, z1),
		    z0_res = svrsubhnb (z0, z1))

/*
** rsubhnb_s16_tied2:
**	rsubhnb	z0\.b, z1\.h, z0\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (rsubhnb_s16_tied2, svint8_t, svint16_t,
		    z0_res = svrsubhnb_s16 (z1, z0),
		    z0_res = svrsubhnb (z1, z0))

/*
** rsubhnb_s16_untied:
**	rsubhnb	z0\.b, z1\.h, z2\.h
**	ret
*/
TEST_TYPE_CHANGE_Z (rsubhnb_s16_untied, svint8_t, svint16_t,
		    z0_res = svrsubhnb_s16 (z1, z2),
		    z0_res = svrsubhnb (z1, z2))

/*
** rsubhnb_w0_s16_tied1:
**	mov	(z[0-9]+\.h), w0
**	rsubhnb	z0\.b, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (rsubhnb_w0_s16_tied1, svint8_t, svint16_t, int16_t,
		     z0_res = svrsubhnb_n_s16 (z0, x0),
		     z0_res = svrsubhnb (z0, x0))

/*
** rsubhnb_w0_s16_untied:
**	mov	(z[0-9]+\.h), w0
**	rsubhnb	z0\.b, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (rsubhnb_w0_s16_untied, svint8_t, svint16_t, int16_t,
		     z0_res = svrsubhnb_n_s16 (z1, x0),
		     z0_res = svrsubhnb (z1, x0))

/*
** rsubhnb_11_s16_tied1:
**	mov	(z[0-9]+\.h), #11
**	rsubhnb	z0\.b, z0\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (rsubhnb_11_s16_tied1, svint8_t, svint16_t,
		    z0_res = svrsubhnb_n_s16 (z0, 11),
		    z0_res = svrsubhnb (z0, 11))

/*
** rsubhnb_11_s16_untied:
**	mov	(z[0-9]+\.h), #11
**	rsubhnb	z0\.b, z1\.h, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (rsubhnb_11_s16_untied, svint8_t, svint16_t,
		    z0_res = svrsubhnb_n_s16 (z1, 11),
		    z0_res = svrsubhnb (z1, 11))
