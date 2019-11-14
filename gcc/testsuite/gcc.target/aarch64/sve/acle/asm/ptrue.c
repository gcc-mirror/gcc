/* { dg-additional-options "-msve-vector-bits=scalable" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ptrue_b8:
**	ptrue	p0\.b, all
**	ret
*/
TEST_P (ptrue_b8,
	p0 = svptrue_b8 (),
	p0 = svptrue_b8 ());

/*
** ptrue_b16:
**	ptrue	p0\.h, all
**	ret
*/
TEST_P (ptrue_b16,
	p0 = svptrue_b16 (),
	p0 = svptrue_b16 ());

/*
** ptrue_b32:
**	ptrue	p0\.s, all
**	ret
*/
TEST_P (ptrue_b32,
	p0 = svptrue_b32 (),
	p0 = svptrue_b32 ());

/*
** ptrue_b64:
**	ptrue	p0\.d, all
**	ret
*/
TEST_P (ptrue_b64,
	p0 = svptrue_b64 (),
	p0 = svptrue_b64 ());
