/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include <stdbool.h>
#include "test_sve_acle.h"

/*
** dup_false_b32:
**	pfalse	p0\.b
**	ret
*/
TEST_UNIFORM_P (dup_false_b32,
		p0 = svdup_n_b32 (false),
		p0 = svdup_b32 (false))

/*
** dup_true_b32:
**	ptrue	p0\.s, all
**	ret
*/
TEST_UNIFORM_P (dup_true_b32,
		p0 = svdup_n_b32 (true),
		p0 = svdup_b32 (true))

/*
** dup_w0_b32:
**	lsl	(x[0-9]+), x0, 63
**	whilelo	p0\.s, xzr, \1
**	ret
*/
TEST_UNIFORM_PS (dup_w0_b32,
		 p0 = svdup_n_b32 (x0),
		 p0 = svdup_b32 (x0))
