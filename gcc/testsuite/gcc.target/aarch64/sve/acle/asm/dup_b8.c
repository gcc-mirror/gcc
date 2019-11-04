/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include <stdbool.h>
#include "test_sve_acle.h"

/*
** dup_false_b8:
**	pfalse	p0\.b
**	ret
*/
TEST_UNIFORM_P (dup_false_b8,
		p0 = svdup_n_b8 (false),
		p0 = svdup_b8 (false))

/*
** dup_true_b8:
**	ptrue	p0\.b, all
**	ret
*/
TEST_UNIFORM_P (dup_true_b8,
		p0 = svdup_n_b8 (true),
		p0 = svdup_b8 (true))

/*
** dup_w0_b8:
**	lsl	(x[0-9]+), x0, 63
**	whilelo	p0\.b, xzr, \1
**	ret
*/
TEST_UNIFORM_PS (dup_w0_b8,
		 p0 = svdup_n_b8 (x0),
		 p0 = svdup_b8 (x0))
