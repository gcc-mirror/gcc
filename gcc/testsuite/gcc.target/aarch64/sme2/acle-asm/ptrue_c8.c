/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** ptrue_pn0:
**	ptrue	pn([8-9]|1[0-5])\.b
**	mov	p0\.b, p\1\.b
**	ret
*/
TEST_PN (ptrue_pn0,
	 pn0 = svptrue_c8 (),
	 pn0 = svptrue_c8 ())

/*
** ptrue_pn7:
**	ptrue	pn([8-9]|1[0-5])\.b
**	mov	p7\.b, p\1\.b
**	ret
*/
TEST_PN (ptrue_pn7,
	 pn7 = svptrue_c8 (),
	 pn7 = svptrue_c8 ())

/*
** ptrue_pn8:
**	ptrue	pn8\.b
**	ret
*/
TEST_PN (ptrue_pn8,
	 pn8 = svptrue_c8 (),
	 pn8 = svptrue_c8 ())

/*
** ptrue_pn15:
**	ptrue	pn15\.b
**	ret
*/
TEST_PN (ptrue_pn15,
	 pn15 = svptrue_c8 (),
	 pn15 = svptrue_c8 ())
