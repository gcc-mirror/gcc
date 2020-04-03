/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ld1rq_f64_base:
**	ld1rqd	z0\.d, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1rq_f64_base, svfloat64_t, float64_t,
	   z0 = svld1rq_f64 (p0, x0),
	   z0 = svld1rq (p0, x0))

/*
** ld1rq_f64_index:
**	ld1rqd	z0\.d, p0/z, \[x0, x1, lsl 3\]
**	ret
*/
TEST_LOAD (ld1rq_f64_index, svfloat64_t, float64_t,
	   z0 = svld1rq_f64 (p0, x0 + x1),
	   z0 = svld1rq (p0, x0 + x1))

/*
** ld1rq_f64_1:
**	add	(x[0-9]+), x0, #?8
**	ld1rqd	z0\.d, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_f64_1, svfloat64_t, float64_t,
	   z0 = svld1rq_f64 (p0, x0 + 1),
	   z0 = svld1rq (p0, x0 + 1))

/*
** ld1rq_f64_2:
**	ld1rqd	z0\.d, p0/z, \[x0, #?16\]
**	ret
*/
TEST_LOAD (ld1rq_f64_2, svfloat64_t, float64_t,
	   z0 = svld1rq_f64 (p0, x0 + 2),
	   z0 = svld1rq (p0, x0 + 2))

/*
** ld1rq_f64_14:
**	ld1rqd	z0\.d, p0/z, \[x0, #?112\]
**	ret
*/
TEST_LOAD (ld1rq_f64_14, svfloat64_t, float64_t,
	   z0 = svld1rq_f64 (p0, x0 + 14),
	   z0 = svld1rq (p0, x0 + 14))

/*
** ld1rq_f64_16:
**	add	(x[0-9]+), x0, #?128
**	ld1rqd	z0\.d, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_f64_16, svfloat64_t, float64_t,
	   z0 = svld1rq_f64 (p0, x0 + 16),
	   z0 = svld1rq (p0, x0 + 16))

/*
** ld1rq_f64_m1:
**	sub	(x[0-9]+), x0, #?8
**	ld1rqd	z0\.d, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_f64_m1, svfloat64_t, float64_t,
	   z0 = svld1rq_f64 (p0, x0 - 1),
	   z0 = svld1rq (p0, x0 - 1))

/*
** ld1rq_f64_m2:
**	ld1rqd	z0\.d, p0/z, \[x0, #?-16\]
**	ret
*/
TEST_LOAD (ld1rq_f64_m2, svfloat64_t, float64_t,
	   z0 = svld1rq_f64 (p0, x0 - 2),
	   z0 = svld1rq (p0, x0 - 2))

/*
** ld1rq_f64_m16:
**	ld1rqd	z0\.d, p0/z, \[x0, #?-128\]
**	ret
*/
TEST_LOAD (ld1rq_f64_m16, svfloat64_t, float64_t,
	   z0 = svld1rq_f64 (p0, x0 - 16),
	   z0 = svld1rq (p0, x0 - 16))

/*
** ld1rq_f64_m18:
**	sub	(x[0-9]+), x0, #?144
**	ld1rqd	z0\.d, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_f64_m18, svfloat64_t, float64_t,
	   z0 = svld1rq_f64 (p0, x0 - 18),
	   z0 = svld1rq (p0, x0 - 18))
