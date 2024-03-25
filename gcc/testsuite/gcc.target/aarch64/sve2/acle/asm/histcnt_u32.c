/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** histcnt_u32_z_tied1:
**	histcnt	z0\.s, p0/z, z0\.s, z1\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (histcnt_u32_z_tied1, svuint32_t, svuint32_t,
		    z0_res = svhistcnt_u32_z (p0, z0, z1),
		    z0_res = svhistcnt_z (p0, z0, z1))

/*
** histcnt_u32_z_tied2:
**	histcnt	z0\.s, p0/z, z1\.s, z0\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (histcnt_u32_z_tied2, svuint32_t, svuint32_t,
		    z0_res = svhistcnt_u32_z (p0, z1, z0),
		    z0_res = svhistcnt_z (p0, z1, z0))

/*
** histcnt_u32_z_untied:
**	histcnt	z0\.s, p0/z, z1\.s, z2\.s
**	ret
*/
TEST_TYPE_CHANGE_Z (histcnt_u32_z_untied, svuint32_t, svuint32_t,
		    z0_res = svhistcnt_u32_z (p0, z1, z2),
		    z0_res = svhistcnt_z (p0, z1, z2))
