/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** pfalse_pn0:
**	pfalse	p0\.b
**	ret
*/
TEST_PN (pfalse_pn0,
	 pn0 = svpfalse_c (),
	 pn0 = svpfalse_c ())

/*
** pfalse_pn7:
**	pfalse	p7\.b
**	ret
*/
TEST_PN (pfalse_pn7,
	 pn7 = svpfalse_c (),
	 pn7 = svpfalse_c ())

/*
** pfalse_pn8:
**	pfalse	p8\.b
**	ret
*/
TEST_PN (pfalse_pn8,
	 pn8 = svpfalse_c (),
	 pn8 = svpfalse_c ())

/*
** pfalse_pn15:
**	pfalse	p15\.b
**	ret
*/
TEST_PN (pfalse_pn15,
	 pn15 = svpfalse_c (),
	 pn15 = svpfalse_c ())
