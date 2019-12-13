/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ld1rq_s16_base:
**	ld1rqh	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1rq_s16_base, svint16_t, int16_t,
	   z0 = svld1rq_s16 (p0, x0),
	   z0 = svld1rq (p0, x0))

/*
** ld1rq_s16_index:
**	ld1rqh	z0\.h, p0/z, \[x0, x1, lsl 1\]
**	ret
*/
TEST_LOAD (ld1rq_s16_index, svint16_t, int16_t,
	   z0 = svld1rq_s16 (p0, x0 + x1),
	   z0 = svld1rq (p0, x0 + x1))

/*
** ld1rq_s16_1:
**	add	(x[0-9]+), x0, #?2
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s16_1, svint16_t, int16_t,
	   z0 = svld1rq_s16 (p0, x0 + 1),
	   z0 = svld1rq (p0, x0 + 1))

/*
** ld1rq_s16_4:
**	add	(x[0-9]+), x0, #?8
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s16_4, svint16_t, int16_t,
	   z0 = svld1rq_s16 (p0, x0 + 4),
	   z0 = svld1rq (p0, x0 + 4))

/*
** ld1rq_s16_7:
**	add	(x[0-9]+), x0, #?14
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s16_7, svint16_t, int16_t,
	   z0 = svld1rq_s16 (p0, x0 + 7),
	   z0 = svld1rq (p0, x0 + 7))

/*
** ld1rq_s16_8:
**	ld1rqh	z0\.h, p0/z, \[x0, #?16\]
**	ret
*/
TEST_LOAD (ld1rq_s16_8, svint16_t, int16_t,
	   z0 = svld1rq_s16 (p0, x0 + 8),
	   z0 = svld1rq (p0, x0 + 8))

/*
** ld1rq_s16_56:
**	ld1rqh	z0\.h, p0/z, \[x0, #?112\]
**	ret
*/
TEST_LOAD (ld1rq_s16_56, svint16_t, int16_t,
	   z0 = svld1rq_s16 (p0, x0 + 56),
	   z0 = svld1rq (p0, x0 + 56))

/*
** ld1rq_s16_64:
**	add	(x[0-9]+), x0, #?128
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s16_64, svint16_t, int16_t,
	   z0 = svld1rq_s16 (p0, x0 + 64),
	   z0 = svld1rq (p0, x0 + 64))

/*
** ld1rq_s16_m1:
**	sub	(x[0-9]+), x0, #?2
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s16_m1, svint16_t, int16_t,
	   z0 = svld1rq_s16 (p0, x0 - 1),
	   z0 = svld1rq (p0, x0 - 1))

/*
** ld1rq_s16_m4:
**	sub	(x[0-9]+), x0, #?8
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s16_m4, svint16_t, int16_t,
	   z0 = svld1rq_s16 (p0, x0 - 4),
	   z0 = svld1rq (p0, x0 - 4))

/*
** ld1rq_s16_m7:
**	sub	(x[0-9]+), x0, #?14
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s16_m7, svint16_t, int16_t,
	   z0 = svld1rq_s16 (p0, x0 - 7),
	   z0 = svld1rq (p0, x0 - 7))

/*
** ld1rq_s16_m8:
**	ld1rqh	z0\.h, p0/z, \[x0, #?-16\]
**	ret
*/
TEST_LOAD (ld1rq_s16_m8, svint16_t, int16_t,
	   z0 = svld1rq_s16 (p0, x0 - 8),
	   z0 = svld1rq (p0, x0 - 8))

/*
** ld1rq_s16_m64:
**	ld1rqh	z0\.h, p0/z, \[x0, #?-128\]
**	ret
*/
TEST_LOAD (ld1rq_s16_m64, svint16_t, int16_t,
	   z0 = svld1rq_s16 (p0, x0 - 64),
	   z0 = svld1rq (p0, x0 - 64))

/*
** ld1rq_s16_m72:
**	sub	(x[0-9]+), x0, #?144
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_s16_m72, svint16_t, int16_t,
	   z0 = svld1rq_s16 (p0, x0 - 72),
	   z0 = svld1rq (p0, x0 - 72))
