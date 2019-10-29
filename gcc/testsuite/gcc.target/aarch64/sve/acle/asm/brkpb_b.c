/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** brkpb_b_z_tied1:
**	brkpb	p0\.b, p3/z, p0\.b, p1\.b
**	ret
*/
TEST_UNIFORM_P (brkpb_b_z_tied1,
		p0 = svbrkpb_b_z (p3, p0, p1),
		p0 = svbrkpb_z (p3, p0, p1))

/*
** brkpb_b_z_tied2:
**	brkpb	p0\.b, p3/z, p1\.b, p0\.b
**	ret
*/
TEST_UNIFORM_P (brkpb_b_z_tied2,
		p0 = svbrkpb_b_z (p3, p1, p0),
		p0 = svbrkpb_z (p3, p1, p0))

/*
** brkpb_b_z_untied:
**	brkpb	p0\.b, p3/z, p1\.b, p2\.b
**	ret
*/
TEST_UNIFORM_P (brkpb_b_z_untied,
		p0 = svbrkpb_b_z (p3, p1, p2),
		p0 = svbrkpb_z (p3, p1, p2))
