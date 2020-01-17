/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** histcnt_u64_z_tied1:
**	histcnt	z0\.d, p0/z, z0\.d, z1\.d
**	ret
*/
TEST_TYPE_CHANGE_Z (histcnt_u64_z_tied1, svuint64_t, svuint64_t,
		    z0_res = svhistcnt_u64_z (p0, z0, z1),
		    z0_res = svhistcnt_z (p0, z0, z1))

/*
** histcnt_u64_z_tied2:
**	histcnt	z0\.d, p0/z, z1\.d, z0\.d
**	ret
*/
TEST_TYPE_CHANGE_Z (histcnt_u64_z_tied2, svuint64_t, svuint64_t,
		    z0_res = svhistcnt_u64_z (p0, z1, z0),
		    z0_res = svhistcnt_z (p0, z1, z0))

/*
** histcnt_u64_z_untied:
**	histcnt	z0\.d, p0/z, z1\.d, z2\.d
**	ret
*/
TEST_TYPE_CHANGE_Z (histcnt_u64_z_untied, svuint64_t, svuint64_t,
		    z0_res = svhistcnt_u64_z (p0, z1, z2),
		    z0_res = svhistcnt_z (p0, z1, z2))
