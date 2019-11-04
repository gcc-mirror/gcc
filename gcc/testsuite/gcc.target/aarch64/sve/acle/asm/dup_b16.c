/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include <stdbool.h>
#include "test_sve_acle.h"

/*
** dup_false_b16:
**	pfalse	p0\.b
**	ret
*/
TEST_UNIFORM_P (dup_false_b16,
		p0 = svdup_n_b16 (false),
		p0 = svdup_b16 (false))

/*
** dup_true_b16:
**	ptrue	p0\.h, all
**	ret
*/
TEST_UNIFORM_P (dup_true_b16,
		p0 = svdup_n_b16 (true),
		p0 = svdup_b16 (true))

/*
** dup_w0_b16:
**	lsl	(x[0-9]+), x0, 63
**	whilelo	p0\.h, xzr, \1
**	ret
*/
TEST_UNIFORM_PS (dup_w0_b16,
		 p0 = svdup_n_b16 (x0),
		 p0 = svdup_b16 (x0))
