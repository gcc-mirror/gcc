/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_P (brkn_b_z_tied1,
		p0 = svbrkn_b_z (p3, p0, p1),
		p0 = svbrkn_z (p3, p0, p1))

/*
** brkn_b_z_tied2:
**	brkn	p0\.b, p3/z, p1\.b, p0\.b
**	ret
*/
TEST_UNIFORM_P (brkn_b_z_tied2,
		p0 = svbrkn_b_z (p3, p1, p0),
		p0 = svbrkn_z (p3, p1, p0))

/*
** brkn_b_z_untied:
** (
**	mov	p0\.b, p2\.b
**	brkn	p0\.b, p3/z, p1\.b, p0\.b
** |
**	brkn	p2\.b, p3/z, p1\.b, p2\.b
**	mov	p0\.b, p2\.b
** )
**	ret
*/
TEST_UNIFORM_P (brkn_b_z_untied,
		p0 = svbrkn_b_z (p3, p1, p2),
		p0 = svbrkn_z (p3, p1, p2))
