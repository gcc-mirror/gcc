/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ld1rq_s64_base:
**	ld1rqd	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1rq_s64_base, svint64_t, int64_t,
	   z0 = svld1rq_s64 (p0, x0),
	   z0 = svld1rq (p0, x0))

/*
** ld1rq_s64_index:
**	ld1rqd	z0\.d, p0/z, \[x0, x1, lsl 3\]
**	ret
*/
TEST_LOAD (ld1rq_s64_index, svint64_t, int64_t,
	   z0 = svld1rq_s64 (p0, x0 + x1),
	   z0 = svld1rq (p0, x0 + x1))

/*
** ld1rq_s64_1:
**	add	(x[0-9]+), x0, #?8
**	ld1rqd	z0\.d, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s64_1, svint64_t, int64_t,
	   z0 = svld1rq_s64 (p0, x0 + 1),
	   z0 = svld1rq (p0, x0 + 1))

/*
** ld1rq_s64_2:
**	ld1rqd	z0\.d, p0/z, \[x0, #?16\]
**	ret
*/
TEST_LOAD (ld1rq_s64_2, svint64_t, int64_t,
	   z0 = svld1rq_s64 (p0, x0 + 2),
	   z0 = svld1rq (p0, x0 + 2))

/*
** ld1rq_s64_14:
**	ld1rqd	z0\.d, p0/z, \[x0, #?112\]
**	ret
*/
TEST_LOAD (ld1rq_s64_14, svint64_t, int64_t,
	   z0 = svld1rq_s64 (p0, x0 + 14),
	   z0 = svld1rq (p0, x0 + 14))

/*
** ld1rq_s64_16:
**	add	(x[0-9]+), x0, #?128
**	ld1rqd	z0\.d, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s64_16, svint64_t, int64_t,
	   z0 = svld1rq_s64 (p0, x0 + 16),
	   z0 = svld1rq (p0, x0 + 16))

/*
** ld1rq_s64_m1:
**	sub	(x[0-9]+), x0, #?8
**	ld1rqd	z0\.d, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s64_m1, svint64_t, int64_t,
	   z0 = svld1rq_s64 (p0, x0 - 1),
	   z0 = svld1rq (p0, x0 - 1))

/*
** ld1rq_s64_m2:
**	ld1rqd	z0\.d, p0/z, \[x0, #?-16\]
**	ret
*/
TEST_LOAD (ld1rq_s64_m2, svint64_t, int64_t,
	   z0 = svld1rq_s64 (p0, x0 - 2),
	   z0 = svld1rq (p0, x0 - 2))

/*
** ld1rq_s64_m16:
**	ld1rqd	z0\.d, p0/z, \[x0, #?-128\]
**	ret
*/
TEST_LOAD (ld1rq_s64_m16, svint64_t, int64_t,
	   z0 = svld1rq_s64 (p0, x0 - 16),
	   z0 = svld1rq (p0, x0 - 16))

/*
** ld1rq_s64_m18:
**	sub	(x[0-9]+), x0, #?144
**	ld1rqd	z0\.d, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s64_m18, svint64_t, int64_t,
	   z0 = svld1rq_s64 (p0, x0 - 18),
	   z0 = svld1rq (p0, x0 - 18))
