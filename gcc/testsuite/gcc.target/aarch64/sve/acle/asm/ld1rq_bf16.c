/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ld1rq_bf16_base:
**	ld1rqh	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1rq_bf16_base, svbfloat16_t, bfloat16_t,
	   z0 = svld1rq_bf16 (p0, x0),
	   z0 = svld1rq (p0, x0))

/*
** ld1rq_bf16_index:
**	ld1rqh	z0\.h, p0/z, \[x0, x1, lsl 1\]
**	ret
*/
TEST_LOAD (ld1rq_bf16_index, svbfloat16_t, bfloat16_t,
	   z0 = svld1rq_bf16 (p0, x0 + x1),
	   z0 = svld1rq (p0, x0 + x1))

/*
** ld1rq_bf16_1:
**	add	(x[0-9]+), x0, #?2
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_bf16_1, svbfloat16_t, bfloat16_t,
	   z0 = svld1rq_bf16 (p0, x0 + 1),
	   z0 = svld1rq (p0, x0 + 1))

/*
** ld1rq_bf16_4:
**	add	(x[0-9]+), x0, #?8
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_bf16_4, svbfloat16_t, bfloat16_t,
	   z0 = svld1rq_bf16 (p0, x0 + 4),
	   z0 = svld1rq (p0, x0 + 4))

/*
** ld1rq_bf16_7:
**	add	(x[0-9]+), x0, #?14
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_bf16_7, svbfloat16_t, bfloat16_t,
	   z0 = svld1rq_bf16 (p0, x0 + 7),
	   z0 = svld1rq (p0, x0 + 7))

/*
** ld1rq_bf16_8:
**	ld1rqh	z0\.h, p0/z, \[x0, #?16\]
**	ret
*/
TEST_LOAD (ld1rq_bf16_8, svbfloat16_t, bfloat16_t,
	   z0 = svld1rq_bf16 (p0, x0 + 8),
	   z0 = svld1rq (p0, x0 + 8))

/*
** ld1rq_bf16_56:
**	ld1rqh	z0\.h, p0/z, \[x0, #?112\]
**	ret
*/
TEST_LOAD (ld1rq_bf16_56, svbfloat16_t, bfloat16_t,
	   z0 = svld1rq_bf16 (p0, x0 + 56),
	   z0 = svld1rq (p0, x0 + 56))

/*
** ld1rq_bf16_64:
**	add	(x[0-9]+), x0, #?128
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_bf16_64, svbfloat16_t, bfloat16_t,
	   z0 = svld1rq_bf16 (p0, x0 + 64),
	   z0 = svld1rq (p0, x0 + 64))

/*
** ld1rq_bf16_m1:
**	sub	(x[0-9]+), x0, #?2
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_bf16_m1, svbfloat16_t, bfloat16_t,
	   z0 = svld1rq_bf16 (p0, x0 - 1),
	   z0 = svld1rq (p0, x0 - 1))

/*
** ld1rq_bf16_m4:
**	sub	(x[0-9]+), x0, #?8
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_bf16_m4, svbfloat16_t, bfloat16_t,
	   z0 = svld1rq_bf16 (p0, x0 - 4),
	   z0 = svld1rq (p0, x0 - 4))

/*
** ld1rq_bf16_m7:
**	sub	(x[0-9]+), x0, #?14
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_bf16_m7, svbfloat16_t, bfloat16_t,
	   z0 = svld1rq_bf16 (p0, x0 - 7),
	   z0 = svld1rq (p0, x0 - 7))

/*
** ld1rq_bf16_m8:
**	ld1rqh	z0\.h, p0/z, \[x0, #?-16\]
**	ret
*/
TEST_LOAD (ld1rq_bf16_m8, svbfloat16_t, bfloat16_t,
	   z0 = svld1rq_bf16 (p0, x0 - 8),
	   z0 = svld1rq (p0, x0 - 8))

/*
** ld1rq_bf16_m64:
**	ld1rqh	z0\.h, p0/z, \[x0, #?-128\]
**	ret
*/
TEST_LOAD (ld1rq_bf16_m64, svbfloat16_t, bfloat16_t,
	   z0 = svld1rq_bf16 (p0, x0 - 64),
	   z0 = svld1rq (p0, x0 - 64))

/*
** ld1rq_bf16_m72:
**	sub	(x[0-9]+), x0, #?144
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_bf16_m72, svbfloat16_t, bfloat16_t,
	   z0 = svld1rq_bf16 (p0, x0 - 72),
	   z0 = svld1rq (p0, x0 - 72))
