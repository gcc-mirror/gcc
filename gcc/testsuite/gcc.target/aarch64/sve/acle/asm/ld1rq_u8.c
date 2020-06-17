/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ld1rq_u8_base:
**	ld1rqb	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1rq_u8_base, svuint8_t, uint8_t,
	   z0 = svld1rq_u8 (p0, x0),
	   z0 = svld1rq (p0, x0))

/*
** ld1rq_u8_index:
**	ld1rqb	z0\.b, p0/z, \[x0, x1\]
**	ret
*/
TEST_LOAD (ld1rq_u8_index, svuint8_t, uint8_t,
	   z0 = svld1rq_u8 (p0, x0 + x1),
	   z0 = svld1rq (p0, x0 + x1))

/*
** ld1rq_u8_1:
**	add	(x[0-9]+), x0, #?1
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u8_1, svuint8_t, uint8_t,
	   z0 = svld1rq_u8 (p0, x0 + 1),
	   z0 = svld1rq (p0, x0 + 1))

/*
** ld1rq_u8_8:
**	add	(x[0-9]+), x0, #?8
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u8_8, svuint8_t, uint8_t,
	   z0 = svld1rq_u8 (p0, x0 + 8),
	   z0 = svld1rq (p0, x0 + 8))

/*
** ld1rq_u8_15:
**	add	(x[0-9]+), x0, #?15
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u8_15, svuint8_t, uint8_t,
	   z0 = svld1rq_u8 (p0, x0 + 15),
	   z0 = svld1rq (p0, x0 + 15))

/*
** ld1rq_u8_16:
**	ld1rqb	z0\.b, p0/z, \[x0, #?16\]
**	ret
*/
TEST_LOAD (ld1rq_u8_16, svuint8_t, uint8_t,
	   z0 = svld1rq_u8 (p0, x0 + 16),
	   z0 = svld1rq (p0, x0 + 16))

/*
** ld1rq_u8_112:
**	ld1rqb	z0\.b, p0/z, \[x0, #?112\]
**	ret
*/
TEST_LOAD (ld1rq_u8_112, svuint8_t, uint8_t,
	   z0 = svld1rq_u8 (p0, x0 + 112),
	   z0 = svld1rq (p0, x0 + 112))

/*
** ld1rq_u8_128:
**	add	(x[0-9]+), x0, #?128
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u8_128, svuint8_t, uint8_t,
	   z0 = svld1rq_u8 (p0, x0 + 128),
	   z0 = svld1rq (p0, x0 + 128))

/*
** ld1rq_u8_m1:
**	sub	(x[0-9]+), x0, #?1
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u8_m1, svuint8_t, uint8_t,
	   z0 = svld1rq_u8 (p0, x0 - 1),
	   z0 = svld1rq (p0, x0 - 1))

/*
** ld1rq_u8_m8:
**	sub	(x[0-9]+), x0, #?8
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u8_m8, svuint8_t, uint8_t,
	   z0 = svld1rq_u8 (p0, x0 - 8),
	   z0 = svld1rq (p0, x0 - 8))

/*
** ld1rq_u8_m15:
**	sub	(x[0-9]+), x0, #?15
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u8_m15, svuint8_t, uint8_t,
	   z0 = svld1rq_u8 (p0, x0 - 15),
	   z0 = svld1rq (p0, x0 - 15))

/*
** ld1rq_u8_m16:
**	ld1rqb	z0\.b, p0/z, \[x0, #?-16\]
**	ret
*/
TEST_LOAD (ld1rq_u8_m16, svuint8_t, uint8_t,
	   z0 = svld1rq_u8 (p0, x0 - 16),
	   z0 = svld1rq (p0, x0 - 16))

/*
** ld1rq_u8_m128:
**	ld1rqb	z0\.b, p0/z, \[x0, #?-128\]
**	ret
*/
TEST_LOAD (ld1rq_u8_m128, svuint8_t, uint8_t,
	   z0 = svld1rq_u8 (p0, x0 - 128),
	   z0 = svld1rq (p0, x0 - 128))

/*
** ld1rq_u8_m144:
**	sub	(x[0-9]+), x0, #?144
**	ld1rqb	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u8_m144, svuint8_t, uint8_t,
	   z0 = svld1rq_u8 (p0, x0 - 144),
	   z0 = svld1rq (p0, x0 - 144))
