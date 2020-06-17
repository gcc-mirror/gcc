/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** tbl2_u16_tied1:
**	tbl	z0\.h, {z0\.h(?:, | - )z1\.h}, z4\.h
**	ret
*/
TEST_TBL2 (tbl2_u16_tied1, svuint16x2_t, svuint16_t, svuint16_t,
	   z0_res = svtbl2_u16 (z0, z4),
	   z0_res = svtbl2 (z0, z4))

/*
** tbl2_u16_tied2:
**	tbl	z0\.h, {z1\.h(?:, | - )z2\.h}, z0\.h
**	ret
*/
TEST_TBL2_REV (tbl2_u16_tied2, svuint16x2_t, svuint16_t, svuint16_t,
	       z0_res = svtbl2_u16 (z1, z0),
	       z0_res = svtbl2 (z1, z0))

/*
** tbl2_u16_untied:
**	tbl	z0\.h, {z2\.h(?:, | - )z3\.h}, z4\.h
**	ret
*/
TEST_TBL2 (tbl2_u16_untied, svuint16x2_t, svuint16_t, svuint16_t,
	   z0_res = svtbl2_u16 (z2, z4),
	   z0_res = svtbl2 (z2, z4))
