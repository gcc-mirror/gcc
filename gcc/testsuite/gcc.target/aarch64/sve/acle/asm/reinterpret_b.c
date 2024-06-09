/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** reinterpret_b_c_tied1:
**	ret
*/
TEST_DUAL_P_REV (reinterpret_b_c_tied1, svbool_t, svcount_t,
		 p0_res = svreinterpret_b_c (p0),
		 p0_res = svreinterpret_b (p0))

/*
** reinterpret_b_c_untied:
**	mov	p0\.b, p2\.b
**	ret
*/
TEST_DUAL_P (reinterpret_b_c_untied, svbool_t, svcount_t,
	     p0 = svreinterpret_b_c (p2),
	     p0 = svreinterpret_b (p2))
