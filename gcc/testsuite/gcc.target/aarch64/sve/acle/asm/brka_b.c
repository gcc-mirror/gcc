/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** brka_b_m_tied12:
**	brka	p0\.b, p3/m, p0\.b
**	ret
*/
TEST_UNIFORM_P (brka_b_m_tied12,
		p0 = svbrka_b_m (p0, p3, p0),
		p0 = svbrka_m (p0, p3, p0))

/*
** brka_b_m_tied1:
**	brka	p0\.b, p3/m, p1\.b
**	ret
*/
TEST_UNIFORM_P (brka_b_m_tied1,
		p0 = svbrka_b_m (p0, p3, p1),
		p0 = svbrka_m (p0, p3, p1))

/* Bad RA choice: no preferred output sequence.  */
TEST_UNIFORM_P (brka_b_m_tied2,
		p0 = svbrka_b_m (p1, p3, p0),
		p0 = svbrka_m (p1, p3, p0))

/*
** brka_b_m_untied:
** (
**	mov	p0\.b, p2\.b
**	brka	p0\.b, p3/m, p1\.b
** |
**	brka	p2\.b, p3/m, p1\.b
**	mov	p0\.b, p2\.b
** )
**	ret
*/
TEST_UNIFORM_P (brka_b_m_untied,
		p0 = svbrka_b_m (p2, p3, p1),
		p0 = svbrka_m (p2, p3, p1))

/*
** brka_b_z_tied1:
**	brka	p0\.b, p3/z, p0\.b
**	ret
*/
TEST_UNIFORM_P (brka_b_z_tied1,
		p0 = svbrka_b_z (p3, p0),
		p0 = svbrka_z (p3, p0))

/*
** brka_b_z_untied:
**	brka	p0\.b, p3/z, p1\.b
**	ret
*/
TEST_UNIFORM_P (brka_b_z_untied,
		p0 = svbrka_b_z (p3, p1),
		p0 = svbrka_z (p3, p1))
