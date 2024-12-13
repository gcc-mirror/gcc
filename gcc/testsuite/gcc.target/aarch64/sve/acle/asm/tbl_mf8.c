/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** tbl_mf8_tied1:
**	tbl	z0\.b, {z0\.b}, z4\.b
**	ret
*/
TEST_DUAL_Z (tbl_mf8_tied1, svmfloat8_t, svuint8_t,
	     z0 = svtbl_mf8 (z0, z4),
	     z0 = svtbl (z0, z4))

/*
** tbl_mf8_tied2:
**	tbl	z0\.b, {z4\.b}, z0\.b
**	ret
*/
TEST_DUAL_Z_REV (tbl_mf8_tied2, svmfloat8_t, svuint8_t,
		 z0_res = svtbl_mf8 (z4, z0),
		 z0_res = svtbl (z4, z0))

/*
** tbl_mf8_untied:
**	tbl	z0\.b, {z1\.b}, z4\.b
**	ret
*/
TEST_DUAL_Z (tbl_mf8_untied, svmfloat8_t, svuint8_t,
	     z0 = svtbl_mf8 (z1, z4),
	     z0 = svtbl (z1, z4))
