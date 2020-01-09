/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abdlt_s16_tied1:
**	sabdlt	z0\.h, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_s16_tied1, svint16_t, svint8_t,
		    z0_res = svabdlt_s16 (z0, z1),
		    z0_res = svabdlt (z0, z1))

/*
** abdlt_s16_tied2:
**	sabdlt	z0\.h, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_s16_tied2, svint16_t, svint8_t,
		    z0_res = svabdlt_s16 (z1, z0),
		    z0_res = svabdlt (z1, z0))

/*
** abdlt_s16_untied:
**	sabdlt	z0\.h, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_s16_untied, svint16_t, svint8_t,
		    z0_res = svabdlt_s16 (z1, z2),
		    z0_res = svabdlt (z1, z2))

/*
** abdlt_w0_s16_tied1:
**	mov	(z[0-9]+\.b), w0
**	sabdlt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlt_w0_s16_tied1, svint16_t, svint8_t, int8_t,
		     z0_res = svabdlt_n_s16 (z0, x0),
		     z0_res = svabdlt (z0, x0))

/*
** abdlt_w0_s16_untied:
**	mov	(z[0-9]+\.b), w0
**	sabdlt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (abdlt_w0_s16_untied, svint16_t, svint8_t, int8_t,
		     z0_res = svabdlt_n_s16 (z1, x0),
		     z0_res = svabdlt (z1, x0))

/*
** abdlt_11_s16_tied1:
**	mov	(z[0-9]+\.b), #11
**	sabdlt	z0\.h, z0\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_11_s16_tied1, svint16_t, svint8_t,
		    z0_res = svabdlt_n_s16 (z0, 11),
		    z0_res = svabdlt (z0, 11))

/*
** abdlt_11_s16_untied:
**	mov	(z[0-9]+\.b), #11
**	sabdlt	z0\.h, z1\.b, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (abdlt_11_s16_untied, svint16_t, svint8_t,
		    z0_res = svabdlt_n_s16 (z1, 11),
		    z0_res = svabdlt (z1, 11))
