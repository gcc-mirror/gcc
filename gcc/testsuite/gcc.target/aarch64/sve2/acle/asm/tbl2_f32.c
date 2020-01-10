/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** tbl2_f32_tied1:
**	tbl	z0\.s, {z0\.s(?:, | - )z1\.s}, z4\.s
**	ret
*/
TEST_TBL2 (tbl2_f32_tied1, svfloat32x2_t, svfloat32_t, svuint32_t,
	   z0_res = svtbl2_f32 (z0, z4),
	   z0_res = svtbl2 (z0, z4))

/*
** tbl2_f32_tied2:
**	tbl	z0\.s, {z1\.s(?:, | - )z2\.s}, z0\.s
**	ret
*/
TEST_TBL2_REV (tbl2_f32_tied2, svfloat32x2_t, svfloat32_t, svuint32_t,
	       z0_res = svtbl2_f32 (z1, z0),
	       z0_res = svtbl2 (z1, z0))

/*
** tbl2_f32_untied:
**	tbl	z0\.s, {z2\.s(?:, | - )z3\.s}, z4\.s
**	ret
*/
TEST_TBL2 (tbl2_f32_untied, svfloat32x2_t, svfloat32_t, svuint32_t,
	   z0_res = svtbl2_f32 (z2, z4),
	   z0_res = svtbl2 (z2, z4))
