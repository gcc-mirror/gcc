/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ld1rq_mf8_base:
**	ld1rqb	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1rq_mf8_base, svmfloat8_t, mfloat8_t,
	   z0 = svld1rq_mf8 (p0, x0),
	   z0 = svld1rq (p0, x0))

/*
** ld1rq_mf8_index:
**	ld1rqb	z0\.b, p0/z, \[x0, x1\]
**	ret
*/
TEST_LOAD (ld1rq_mf8_index, svmfloat8_t, mfloat8_t,
	   z0 = svld1rq_mf8 (p0, x0 + x1),
	   z0 = svld1rq (p0, x0 + x1))

/*
** ld1rq_mf8_1:
**	add	(x[0-9]+), x0, #?1
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_mf8_1, svmfloat8_t, mfloat8_t,
	   z0 = svld1rq_mf8 (p0, x0 + 1),
	   z0 = svld1rq (p0, x0 + 1))

/*
** ld1rq_mf8_8:
**	add	(x[0-9]+), x0, #?8
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_mf8_8, svmfloat8_t, mfloat8_t,
	   z0 = svld1rq_mf8 (p0, x0 + 8),
	   z0 = svld1rq (p0, x0 + 8))

/*
** ld1rq_mf8_15:
**	add	(x[0-9]+), x0, #?15
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_mf8_15, svmfloat8_t, mfloat8_t,
	   z0 = svld1rq_mf8 (p0, x0 + 15),
	   z0 = svld1rq (p0, x0 + 15))

/*
** ld1rq_mf8_16:
**	ld1rqb	z0\.b, p0/z, \[x0, #?16\]
**	ret
*/
TEST_LOAD (ld1rq_mf8_16, svmfloat8_t, mfloat8_t,
	   z0 = svld1rq_mf8 (p0, x0 + 16),
	   z0 = svld1rq (p0, x0 + 16))

/*
** ld1rq_mf8_112:
**	ld1rqb	z0\.b, p0/z, \[x0, #?112\]
**	ret
*/
TEST_LOAD (ld1rq_mf8_112, svmfloat8_t, mfloat8_t,
	   z0 = svld1rq_mf8 (p0, x0 + 112),
	   z0 = svld1rq (p0, x0 + 112))

/*
** ld1rq_mf8_128:
**	add	(x[0-9]+), x0, #?128
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_mf8_128, svmfloat8_t, mfloat8_t,
	   z0 = svld1rq_mf8 (p0, x0 + 128),
	   z0 = svld1rq (p0, x0 + 128))

/*
** ld1rq_mf8_m1:
**	sub	(x[0-9]+), x0, #?1
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_mf8_m1, svmfloat8_t, mfloat8_t,
	   z0 = svld1rq_mf8 (p0, x0 - 1),
	   z0 = svld1rq (p0, x0 - 1))

/*
** ld1rq_mf8_m8:
**	sub	(x[0-9]+), x0, #?8
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_mf8_m8, svmfloat8_t, mfloat8_t,
	   z0 = svld1rq_mf8 (p0, x0 - 8),
	   z0 = svld1rq (p0, x0 - 8))

/*
** ld1rq_mf8_m15:
**	sub	(x[0-9]+), x0, #?15
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_mf8_m15, svmfloat8_t, mfloat8_t,
	   z0 = svld1rq_mf8 (p0, x0 - 15),
	   z0 = svld1rq (p0, x0 - 15))

/*
** ld1rq_mf8_m16:
**	ld1rqb	z0\.b, p0/z, \[x0, #?-16\]
**	ret
*/
TEST_LOAD (ld1rq_mf8_m16, svmfloat8_t, mfloat8_t,
	   z0 = svld1rq_mf8 (p0, x0 - 16),
	   z0 = svld1rq (p0, x0 - 16))

/*
** ld1rq_mf8_m128:
**	ld1rqb	z0\.b, p0/z, \[x0, #?-128\]
**	ret
*/
TEST_LOAD (ld1rq_mf8_m128, svmfloat8_t, mfloat8_t,
	   z0 = svld1rq_mf8 (p0, x0 - 128),
	   z0 = svld1rq (p0, x0 - 128))

/*
** ld1rq_mf8_m144:
**	sub	(x[0-9]+), x0, #?144
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_mf8_m144, svmfloat8_t, mfloat8_t,
	   z0 = svld1rq_mf8 (p0, x0 - 144),
	   z0 = svld1rq (p0, x0 - 144))
