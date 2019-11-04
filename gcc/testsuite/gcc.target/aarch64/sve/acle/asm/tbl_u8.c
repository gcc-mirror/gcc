/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** tbl_u8_tied1:
**	tbl	z0\.b, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (tbl_u8_tied1, svuint8_t, svuint8_t,
	     z0 = svtbl_u8 (z0, z4),
	     z0 = svtbl (z0, z4))

/*
** tbl_u8_tied2:
**	tbl	z0\.b, z4\.b, z0\.b
**	ret
*/
TEST_DUAL_Z_REV (tbl_u8_tied2, svuint8_t, svuint8_t,
		 z0_res = svtbl_u8 (z4, z0),
		 z0_res = svtbl (z4, z0))

/*
** tbl_u8_untied:
**	tbl	z0\.b, z1\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (tbl_u8_untied, svuint8_t, svuint8_t,
	     z0 = svtbl_u8 (z1, z4),
	     z0 = svtbl (z1, z4))
