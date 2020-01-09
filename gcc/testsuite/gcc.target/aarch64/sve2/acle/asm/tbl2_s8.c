/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** tbl2_s8_tied1:
**	tbl	z0\.b, {z0\.b(?:, | - )z1\.b}, z4\.b
**	ret
*/
TEST_TBL2 (tbl2_s8_tied1, svint8x2_t, svint8_t, svuint8_t,
	   z0_res = svtbl2_s8 (z0, z4),
	   z0_res = svtbl2 (z0, z4))

/*
** tbl2_s8_tied2:
**	tbl	z0\.b, {z1\.b(?:, | - )z2\.b}, z0\.b
**	ret
*/
TEST_TBL2_REV (tbl2_s8_tied2, svint8x2_t, svint8_t, svuint8_t,
	       z0_res = svtbl2_s8 (z1, z0),
	       z0_res = svtbl2 (z1, z0))

/*
** tbl2_s8_untied:
**	tbl	z0\.b, {z2\.b(?:, | - )z3\.b}, z4\.b
**	ret
*/
TEST_TBL2 (tbl2_s8_untied, svint8x2_t, svint8_t, svuint8_t,
	   z0_res = svtbl2_s8 (z2, z4),
	   z0_res = svtbl2 (z2, z4))
