/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** pnext_b32_tied1:
**	pnext	p0\.s, p3, p0\.s
**	ret
*/
TEST_UNIFORM_P (pnext_b32_tied1,
		p0 = svpnext_b32 (p3, p0),
		p0 = svpnext_b32 (p3, p0))

/*
** pnext_b32_untied:
**	mov	p0\.b, p1\.b
**	pnext	p0\.s, p3, p0\.s
**	ret
*/
TEST_UNIFORM_P (pnext_b32_untied,
		p0 = svpnext_b32 (p3, p1),
		p0 = svpnext_b32 (p3, p1))
