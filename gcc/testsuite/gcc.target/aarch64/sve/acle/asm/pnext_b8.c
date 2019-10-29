/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** pnext_b8_tied1:
**	pnext	p0\.b, p3, p0\.b
**	ret
*/
TEST_UNIFORM_P (pnext_b8_tied1,
		p0 = svpnext_b8 (p3, p0),
		p0 = svpnext_b8 (p3, p0))

/*
** pnext_b8_untied:
**	mov	p0\.b, p1\.b
**	pnext	p0\.b, p3, p0\.b
**	ret
*/
TEST_UNIFORM_P (pnext_b8_untied,
		p0 = svpnext_b8 (p3, p1),
		p0 = svpnext_b8 (p3, p1))
