/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** tbl_u16_tied1:
**	tbl	z0\.h, {z0\.h}, z4\.h
**	ret
*/
TEST_DUAL_Z (tbl_u16_tied1, svuint16_t, svuint16_t,
	     z0 = svtbl_u16 (z0, z4),
	     z0 = svtbl (z0, z4))

/*
** tbl_u16_tied2:
**	tbl	z0\.h, {z4\.h}, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (tbl_u16_tied2, svuint16_t, svuint16_t,
		 z0_res = svtbl_u16 (z4, z0),
		 z0_res = svtbl (z4, z0))

/*
** tbl_u16_untied:
**	tbl	z0\.h, {z1\.h}, z4\.h
**	ret
*/
TEST_DUAL_Z (tbl_u16_untied, svuint16_t, svuint16_t,
	     z0 = svtbl_u16 (z1, z4),
	     z0 = svtbl (z1, z4))
