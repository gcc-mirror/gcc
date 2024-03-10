/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** pfirst_b_tied1:
**	pfirst	p0\.b, p3, p0\.b
**	ret
*/
TEST_UNIFORM_P (pfirst_b_tied1,
		p0 = svpfirst_b (p3, p0),
		p0 = svpfirst (p3, p0))

/*
** pfirst_b_untied:
** (
**	mov	p0\.b, p1\.b
**	pfirst	p0\.b, p3, p0\.b
** |
**	pfirst	p1\.b, p3, p1\.b
**	mov	p0\.b, p1\.b
** )
**	ret
*/
TEST_UNIFORM_P (pfirst_b_untied,
		p0 = svpfirst_b (p3, p1),
		p0 = svpfirst (p3, p1))
