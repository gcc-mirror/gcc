/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** tbl2_s32_tied1:
**	tbl	z0\.s, {z0\.s(?:, | - )z1\.s}, z4\.s
**	ret
*/
TEST_TBL2 (tbl2_s32_tied1, svint32x2_t, svint32_t, svuint32_t,
	   z0_res = svtbl2_s32 (z0, z4),
	   z0_res = svtbl2 (z0, z4))

/*
** tbl2_s32_tied2:
**	tbl	z0\.s, {z1\.s(?:, | - )z2\.s}, z0\.s
**	ret
*/
TEST_TBL2_REV (tbl2_s32_tied2, svint32x2_t, svint32_t, svuint32_t,
	       z0_res = svtbl2_s32 (z1, z0),
	       z0_res = svtbl2 (z1, z0))

/*
** tbl2_s32_untied:
**	tbl	z0\.s, {z2\.s(?:, | - )z3\.s}, z4\.s
**	ret
*/
TEST_TBL2 (tbl2_s32_untied, svint32x2_t, svint32_t, svuint32_t,
	   z0_res = svtbl2_s32 (z2, z4),
	   z0_res = svtbl2 (z2, z4))
