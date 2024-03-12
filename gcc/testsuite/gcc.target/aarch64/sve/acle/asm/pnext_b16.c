/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** pnext_b16_tied1:
**	pnext	p0\.h, p3, p0\.h
**	ret
*/
TEST_UNIFORM_P (pnext_b16_tied1,
		p0 = svpnext_b16 (p3, p0),
		p0 = svpnext_b16 (p3, p0))

/*
** pnext_b16_untied:
** (
**	mov	p0\.b, p1\.b
**	pnext	p0\.h, p3, p0\.h
** |
**	pnext	p1\.h, p3, p1\.h
**	mov	p0\.b, p1\.b
** )
**	ret
*/
TEST_UNIFORM_P (pnext_b16_untied,
		p0 = svpnext_b16 (p3, p1),
		p0 = svpnext_b16 (p3, p1))
