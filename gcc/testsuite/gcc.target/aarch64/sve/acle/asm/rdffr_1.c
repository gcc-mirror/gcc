/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** setffr_rdffr_1:
**	ptrue	p0\.b, all
**	ret
*/
TEST_UNIFORM_P_SINGLE (setffr_rdffr_1,
		       svsetffr ();
		       p0 = svrdffr ());

/*
** setffr_rdffr_2:
**	ret
*/
TEST_UNIFORM_P_SINGLE (setffr_rdffr_2,
		       svsetffr ();
		       svrdffr ());

/*
** setffr_rdffr_3:
**	ptrue	p0\.b, all
**	ret
*/
TEST_UNIFORM_P_SINGLE (setffr_rdffr_3,
		       svsetffr ();
		       svsetffr ();
		       svrdffr ();
		       p0 = svrdffr ());

/*
** wrffr_rdffr_1:
**	mov	p0\.b, p1\.b
**	ret
*/
TEST_UNIFORM_P_SINGLE (wrffr_rdffr_1,
		       svwrffr (p1);
		       p0 = svrdffr ());

/*
** wrffr_rdffr_2:
**	ret
*/
TEST_UNIFORM_P_SINGLE (wrffr_rdffr_2,
		       svwrffr (p1);
		       svrdffr ());

/*
** wrffr_rdffr_3:
**	mov	p0\.b, p2\.b
**	ret
*/
TEST_UNIFORM_P_SINGLE (wrffr_rdffr_3,
		       svwrffr (p1);
		       svwrffr (p2);
		       svrdffr ();
		       p0 = svrdffr ());
