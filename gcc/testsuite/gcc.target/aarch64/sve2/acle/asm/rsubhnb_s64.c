/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rsubhnb_s64_tied1:
**	rsubhnb	z0\.s, z0\.d, z1\.d
**	ret
*/
TEST_TYPE_CHANGE_Z (rsubhnb_s64_tied1, svint32_t, svint64_t,
		    z0_res = svrsubhnb_s64 (z0, z1),
		    z0_res = svrsubhnb (z0, z1))

/*
** rsubhnb_s64_tied2:
**	rsubhnb	z0\.s, z1\.d, z0\.d
**	ret
*/
TEST_TYPE_CHANGE_Z (rsubhnb_s64_tied2, svint32_t, svint64_t,
		    z0_res = svrsubhnb_s64 (z1, z0),
		    z0_res = svrsubhnb (z1, z0))

/*
** rsubhnb_s64_untied:
**	rsubhnb	z0\.s, z1\.d, z2\.d
**	ret
*/
TEST_TYPE_CHANGE_Z (rsubhnb_s64_untied, svint32_t, svint64_t,
		    z0_res = svrsubhnb_s64 (z1, z2),
		    z0_res = svrsubhnb (z1, z2))

/*
** rsubhnb_x0_s64_tied1:
**	mov	(z[0-9]+\.d), x0
**	rsubhnb	z0\.s, z0\.d, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (rsubhnb_x0_s64_tied1, svint32_t, svint64_t, int64_t,
		     z0_res = svrsubhnb_n_s64 (z0, x0),
		     z0_res = svrsubhnb (z0, x0))

/*
** rsubhnb_x0_s64_untied:
**	mov	(z[0-9]+\.d), x0
**	rsubhnb	z0\.s, z1\.d, \1
**	ret
*/
TEST_TYPE_CHANGE_ZX (rsubhnb_x0_s64_untied, svint32_t, svint64_t, int64_t,
		     z0_res = svrsubhnb_n_s64 (z1, x0),
		     z0_res = svrsubhnb (z1, x0))

/*
** rsubhnb_11_s64_tied1:
**	mov	(z[0-9]+\.d), #11
**	rsubhnb	z0\.s, z0\.d, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (rsubhnb_11_s64_tied1, svint32_t, svint64_t,
		    z0_res = svrsubhnb_n_s64 (z0, 11),
		    z0_res = svrsubhnb (z0, 11))

/*
** rsubhnb_11_s64_untied:
**	mov	(z[0-9]+\.d), #11
**	rsubhnb	z0\.s, z1\.d, \1
**	ret
*/
TEST_TYPE_CHANGE_Z (rsubhnb_11_s64_untied, svint32_t, svint64_t,
		    z0_res = svrsubhnb_n_s64 (z1, 11),
		    z0_res = svrsubhnb (z1, 11))
