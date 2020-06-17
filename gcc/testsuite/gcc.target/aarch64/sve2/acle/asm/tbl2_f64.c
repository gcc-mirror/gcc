/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** tbl2_f64_tied1:
**	tbl	z0\.d, {z0\.d(?:, | - )z1\.d}, z4\.d
**	ret
*/
TEST_TBL2 (tbl2_f64_tied1, svfloat64x2_t, svfloat64_t, svuint64_t,
	   z0_res = svtbl2_f64 (z0, z4),
	   z0_res = svtbl2 (z0, z4))

/*
** tbl2_f64_tied2:
**	tbl	z0\.d, {z1\.d(?:, | - )z2\.d}, z0\.d
**	ret
*/
TEST_TBL2_REV (tbl2_f64_tied2, svfloat64x2_t, svfloat64_t, svuint64_t,
	       z0_res = svtbl2_f64 (z1, z0),
	       z0_res = svtbl2 (z1, z0))

/*
** tbl2_f64_untied:
**	tbl	z0\.d, {z2\.d(?:, | - )z3\.d}, z4\.d
**	ret
*/
TEST_TBL2 (tbl2_f64_untied, svfloat64x2_t, svfloat64_t, svuint64_t,
	   z0_res = svtbl2_f64 (z2, z4),
	   z0_res = svtbl2 (z2, z4))
