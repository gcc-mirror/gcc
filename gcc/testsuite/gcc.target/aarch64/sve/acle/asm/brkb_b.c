/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** brkb_b_m_tied12:
**	brkb	p0\.b, p3/m, p0\.b
**	ret
*/
TEST_UNIFORM_P (brkb_b_m_tied12,
		p0 = svbrkb_b_m (p0, p3, p0),
		p0 = svbrkb_m (p0, p3, p0))

/*
** brkb_b_m_tied1:
**	brkb	p0\.b, p3/m, p1\.b
**	ret
*/
TEST_UNIFORM_P (brkb_b_m_tied1,
		p0 = svbrkb_b_m (p0, p3, p1),
		p0 = svbrkb_m (p0, p3, p1))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_P (brkb_b_m_tied2,
		p0 = svbrkb_b_m (p1, p3, p0),
		p0 = svbrkb_m (p1, p3, p0))

/*
** brkb_b_m_untied:
** (
**	mov	p0\.b, p2\.b
**	brkb	p0\.b, p3/m, p1\.b
** |
**	brkb	p2\.b, p3/m, p1\.b
**	mov	p0\.b, p2\.b
** )
**	ret
*/
TEST_UNIFORM_P (brkb_b_m_untied,
		p0 = svbrkb_b_m (p2, p3, p1),
		p0 = svbrkb_m (p2, p3, p1))

/*
** brkb_b_z_tied1:
**	brkb	p0\.b, p3/z, p0\.b
**	ret
*/
TEST_UNIFORM_P (brkb_b_z_tied1,
		p0 = svbrkb_b_z (p3, p0),
		p0 = svbrkb_z (p3, p0))

/*
** brkb_b_z_untied:
**	brkb	p0\.b, p3/z, p1\.b
**	ret
*/
TEST_UNIFORM_P (brkb_b_z_untied,
		p0 = svbrkb_b_z (p3, p1),
		p0 = svbrkb_z (p3, p1))
