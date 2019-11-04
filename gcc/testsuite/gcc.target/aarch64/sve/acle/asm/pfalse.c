/* { dg-additional-options "-msve-vector-bits=scalable" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** pfalse_b:
**	pfalse	p0\.b
**	ret
*/
TEST_P (pfalse_b,
	p0 = svpfalse_b (),
	p0 = svpfalse ());
