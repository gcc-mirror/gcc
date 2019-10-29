/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include <stdbool.h>
#include "test_sve_acle.h"

/*
** dup_false_b64:
**	pfalse	p0\.b
**	ret
*/
TEST_UNIFORM_P (dup_false_b64,
		p0 = svdup_n_b64 (false),
		p0 = svdup_b64 (false))

/*
** dup_true_b64:
**	ptrue	p0\.d, all
**	ret
*/
TEST_UNIFORM_P (dup_true_b64,
		p0 = svdup_n_b64 (true),
		p0 = svdup_b64 (true))

/*
** dup_w0_b64:
**	lsl	(x[0-9]+), x0, 63
**	whilelo	p0\.d, xzr, \1
**	ret
*/
TEST_UNIFORM_PS (dup_w0_b64,
		 p0 = svdup_n_b64 (x0),
		 p0 = svdup_b64 (x0))
