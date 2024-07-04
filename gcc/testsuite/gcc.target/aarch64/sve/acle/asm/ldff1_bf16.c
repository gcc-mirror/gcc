/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldff1_bf16_base:
**	ldff1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_bf16_base, svbfloat16_t, bfloat16_t,
	   z0 = svldff1_bf16 (p0, x0),
	   z0 = svldff1 (p0, x0))

/*
** ldff1_bf16_index:
**	ldff1h	z0\.h, p0/z, \[x0, x1, lsl 1\]
**	ret
*/
TEST_LOAD (ldff1_bf16_index, svbfloat16_t, bfloat16_t,
	   z0 = svldff1_bf16 (p0, x0 + x1),
	   z0 = svldff1 (p0, x0 + x1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1_bf16_1:
**	incb	x0
**	ldff1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_bf16_1, svbfloat16_t, bfloat16_t,
	   z0 = svldff1_bf16 (p0, x0 + svcnth ()),
	   z0 = svldff1 (p0, x0 + svcnth ()))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1_bf16_m1:
**	decb	x0
**	ldff1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_bf16_m1, svbfloat16_t, bfloat16_t,
	   z0 = svldff1_bf16 (p0, x0 - svcnth ()),
	   z0 = svldff1 (p0, x0 - svcnth ()))

/*
** ldff1_vnum_bf16_0:
**	ldff1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_vnum_bf16_0, svbfloat16_t, bfloat16_t,
	   z0 = svldff1_vnum_bf16 (p0, x0, 0),
	   z0 = svldff1_vnum (p0, x0, 0))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1_vnum_bf16_1:
**	incb	x0
**	ldff1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_vnum_bf16_1, svbfloat16_t, bfloat16_t,
	   z0 = svldff1_vnum_bf16 (p0, x0, 1),
	   z0 = svldff1_vnum (p0, x0, 1))

/* Moving the constant into a register would also be OK.  */
/*
** ldff1_vnum_bf16_m1:
**	decb	x0
**	ldff1h	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ldff1_vnum_bf16_m1, svbfloat16_t, bfloat16_t,
	   z0 = svldff1_vnum_bf16 (p0, x0, -1),
	   z0 = svldff1_vnum (p0, x0, -1))

/* Using MUL to calculate an index would also be OK.  */
/*
** ldff1_vnum_bf16_x1:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (x1, \1|\1, x1), x0
**	ldff1h	z0\.h, p0/z, \[\2\]
**	ret
*/
TEST_LOAD (ldff1_vnum_bf16_x1, svbfloat16_t, bfloat16_t,
	   z0 = svldff1_vnum_bf16 (p0, x0, x1),
	   z0 = svldff1_vnum (p0, x0, x1))
