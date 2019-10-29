/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ld1rq_s8_base:
**	ld1rqb	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1rq_s8_base, svint8_t, int8_t,
	   z0 = svld1rq_s8 (p0, x0),
	   z0 = svld1rq (p0, x0))

/*
** ld1rq_s8_index:
**	ld1rqb	z0\.b, p0/z, \[x0, x1\]
**	ret
*/
TEST_LOAD (ld1rq_s8_index, svint8_t, int8_t,
	   z0 = svld1rq_s8 (p0, x0 + x1),
	   z0 = svld1rq (p0, x0 + x1))

/*
** ld1rq_s8_1:
**	add	(x[0-9]+), x0, #?1
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s8_1, svint8_t, int8_t,
	   z0 = svld1rq_s8 (p0, x0 + 1),
	   z0 = svld1rq (p0, x0 + 1))

/*
** ld1rq_s8_8:
**	add	(x[0-9]+), x0, #?8
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s8_8, svint8_t, int8_t,
	   z0 = svld1rq_s8 (p0, x0 + 8),
	   z0 = svld1rq (p0, x0 + 8))

/*
** ld1rq_s8_15:
**	add	(x[0-9]+), x0, #?15
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s8_15, svint8_t, int8_t,
	   z0 = svld1rq_s8 (p0, x0 + 15),
	   z0 = svld1rq (p0, x0 + 15))

/*
** ld1rq_s8_16:
**	ld1rqb	z0\.b, p0/z, \[x0, #?16\]
**	ret
*/
TEST_LOAD (ld1rq_s8_16, svint8_t, int8_t,
	   z0 = svld1rq_s8 (p0, x0 + 16),
	   z0 = svld1rq (p0, x0 + 16))

/*
** ld1rq_s8_112:
**	ld1rqb	z0\.b, p0/z, \[x0, #?112\]
**	ret
*/
TEST_LOAD (ld1rq_s8_112, svint8_t, int8_t,
	   z0 = svld1rq_s8 (p0, x0 + 112),
	   z0 = svld1rq (p0, x0 + 112))

/*
** ld1rq_s8_128:
**	add	(x[0-9]+), x0, #?128
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s8_128, svint8_t, int8_t,
	   z0 = svld1rq_s8 (p0, x0 + 128),
	   z0 = svld1rq (p0, x0 + 128))

/*
** ld1rq_s8_m1:
**	sub	(x[0-9]+), x0, #?1
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s8_m1, svint8_t, int8_t,
	   z0 = svld1rq_s8 (p0, x0 - 1),
	   z0 = svld1rq (p0, x0 - 1))

/*
** ld1rq_s8_m8:
**	sub	(x[0-9]+), x0, #?8
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s8_m8, svint8_t, int8_t,
	   z0 = svld1rq_s8 (p0, x0 - 8),
	   z0 = svld1rq (p0, x0 - 8))

/*
** ld1rq_s8_m15:
**	sub	(x[0-9]+), x0, #?15
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s8_m15, svint8_t, int8_t,
	   z0 = svld1rq_s8 (p0, x0 - 15),
	   z0 = svld1rq (p0, x0 - 15))

/*
** ld1rq_s8_m16:
**	ld1rqb	z0\.b, p0/z, \[x0, #?-16\]
**	ret
*/
TEST_LOAD (ld1rq_s8_m16, svint8_t, int8_t,
	   z0 = svld1rq_s8 (p0, x0 - 16),
	   z0 = svld1rq (p0, x0 - 16))

/*
** ld1rq_s8_m128:
**	ld1rqb	z0\.b, p0/z, \[x0, #?-128\]
**	ret
*/
TEST_LOAD (ld1rq_s8_m128, svint8_t, int8_t,
	   z0 = svld1rq_s8 (p0, x0 - 128),
	   z0 = svld1rq (p0, x0 - 128))

/*
** ld1rq_s8_m144:
**	sub	(x[0-9]+), x0, #?144
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s8_m144, svint8_t, int8_t,
	   z0 = svld1rq_s8 (p0, x0 - 144),
	   z0 = svld1rq (p0, x0 - 144))
