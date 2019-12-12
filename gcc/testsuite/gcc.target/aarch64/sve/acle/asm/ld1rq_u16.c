/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** ld1rq_u16_base:
**	ld1rqh	z0\.h, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1rq_u16_base, svuint16_t, uint16_t,
	   z0 = svld1rq_u16 (p0, x0),
	   z0 = svld1rq (p0, x0))

/*
** ld1rq_u16_index:
**	ld1rqh	z0\.h, p0/z, \[x0, x1, lsl 1\]
**	ret
*/
TEST_LOAD (ld1rq_u16_index, svuint16_t, uint16_t,
	   z0 = svld1rq_u16 (p0, x0 + x1),
	   z0 = svld1rq (p0, x0 + x1))

/*
** ld1rq_u16_1:
**	add	(x[0-9]+), x0, #?2
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u16_1, svuint16_t, uint16_t,
	   z0 = svld1rq_u16 (p0, x0 + 1),
	   z0 = svld1rq (p0, x0 + 1))

/*
** ld1rq_u16_4:
**	add	(x[0-9]+), x0, #?8
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u16_4, svuint16_t, uint16_t,
	   z0 = svld1rq_u16 (p0, x0 + 4),
	   z0 = svld1rq (p0, x0 + 4))

/*
** ld1rq_u16_7:
**	add	(x[0-9]+), x0, #?14
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u16_7, svuint16_t, uint16_t,
	   z0 = svld1rq_u16 (p0, x0 + 7),
	   z0 = svld1rq (p0, x0 + 7))

/*
** ld1rq_u16_8:
**	ld1rqh	z0\.h, p0/z, \[x0, #?16\]
**	ret
*/
TEST_LOAD (ld1rq_u16_8, svuint16_t, uint16_t,
	   z0 = svld1rq_u16 (p0, x0 + 8),
	   z0 = svld1rq (p0, x0 + 8))

/*
** ld1rq_u16_56:
**	ld1rqh	z0\.h, p0/z, \[x0, #?112\]
**	ret
*/
TEST_LOAD (ld1rq_u16_56, svuint16_t, uint16_t,
	   z0 = svld1rq_u16 (p0, x0 + 56),
	   z0 = svld1rq (p0, x0 + 56))

/*
** ld1rq_u16_64:
**	add	(x[0-9]+), x0, #?128
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u16_64, svuint16_t, uint16_t,
	   z0 = svld1rq_u16 (p0, x0 + 64),
	   z0 = svld1rq (p0, x0 + 64))

/*
** ld1rq_u16_m1:
**	sub	(x[0-9]+), x0, #?2
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u16_m1, svuint16_t, uint16_t,
	   z0 = svld1rq_u16 (p0, x0 - 1),
	   z0 = svld1rq (p0, x0 - 1))

/*
** ld1rq_u16_m4:
**	sub	(x[0-9]+), x0, #?8
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u16_m4, svuint16_t, uint16_t,
	   z0 = svld1rq_u16 (p0, x0 - 4),
	   z0 = svld1rq (p0, x0 - 4))

/*
** ld1rq_u16_m7:
**	sub	(x[0-9]+), x0, #?14
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u16_m7, svuint16_t, uint16_t,
	   z0 = svld1rq_u16 (p0, x0 - 7),
	   z0 = svld1rq (p0, x0 - 7))

/*
** ld1rq_u16_m8:
**	ld1rqh	z0\.h, p0/z, \[x0, #?-16\]
**	ret
*/
TEST_LOAD (ld1rq_u16_m8, svuint16_t, uint16_t,
	   z0 = svld1rq_u16 (p0, x0 - 8),
	   z0 = svld1rq (p0, x0 - 8))

/*
** ld1rq_u16_m64:
**	ld1rqh	z0\.h, p0/z, \[x0, #?-128\]
**	ret
*/
TEST_LOAD (ld1rq_u16_m64, svuint16_t, uint16_t,
	   z0 = svld1rq_u16 (p0, x0 - 64),
	   z0 = svld1rq (p0, x0 - 64))

/*
** ld1rq_u16_m72:
**	sub	(x[0-9]+), x0, #?144
**	ld1rqh	z0\.h, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u16_m72, svuint16_t, uint16_t,
	   z0 = svld1rq_u16 (p0, x0 - 72),
	   z0 = svld1rq (p0, x0 - 72))
