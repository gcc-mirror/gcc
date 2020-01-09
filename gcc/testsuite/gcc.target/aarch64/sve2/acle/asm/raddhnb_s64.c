/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** raddhnb_s64_tied1:
**	raddhnb	z0\.s, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_TYPE_CHANGE_Z (raddhnb_s64_tied1, svint32_t, svint64_t,
		    z0_res = svraddhnb_s64 (z0, z1),
		    z0_res = svraddhnb (z0, z1))

/*
** raddhnb_s64_tied2:
**	raddhnb	z0\.s, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_TYPE_CHANGE_Z (raddhnb_s64_tied2, svint32_t, svint64_t,
		    z0_res = svraddhnb_s64 (z1, z0),
		    z0_res = svraddhnb (z1, z0))

/*
** raddhnb_s64_untied:
**	raddhnb	z0\.s, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_TYPE_CHANGE_Z (raddhnb_s64_untied, svint32_t, svint64_t,
		    z0_res = svraddhnb_s64 (z1, z2),
		    z0_res = svraddhnb (z1, z2))

/*
** raddhnb_x0_s64_tied1:
**	mov	(z[0-9]+\.d), x0
**	raddhnb	z0\.s, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_TYPE_CHANGE_ZX (raddhnb_x0_s64_tied1, svint32_t, svint64_t, int64_t,
		     z0_res = svraddhnb_n_s64 (z0, x0),
		     z0_res = svraddhnb (z0, x0))

/*
** raddhnb_x0_s64_untied:
**	mov	(z[0-9]+\.d), x0
**	raddhnb	z0\.s, (z1\.d, \1|\1, z1\.d)
**	ret
*/
TEST_TYPE_CHANGE_ZX (raddhnb_x0_s64_untied, svint32_t, svint64_t, int64_t,
		     z0_res = svraddhnb_n_s64 (z1, x0),
		     z0_res = svraddhnb (z1, x0))

/*
** raddhnb_11_s64_tied1:
**	mov	(z[0-9]+\.d), #11
**	raddhnb	z0\.s, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_TYPE_CHANGE_Z (raddhnb_11_s64_tied1, svint32_t, svint64_t,
		    z0_res = svraddhnb_n_s64 (z0, 11),
		    z0_res = svraddhnb (z0, 11))

/*
** raddhnb_11_s64_untied:
**	mov	(z[0-9]+\.d), #11
**	raddhnb	z0\.s, (z1\.d, \1|\1, z1\.d)
**	ret
*/
TEST_TYPE_CHANGE_Z (raddhnb_11_s64_untied, svint32_t, svint64_t,
		    z0_res = svraddhnb_n_s64 (z1, 11),
		    z0_res = svraddhnb (z1, 11))
