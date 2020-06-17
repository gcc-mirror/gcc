/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ld1rq_u32_base:
**	ld1rqw	z0\.s, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1rq_u32_base, svuint32_t, uint32_t,
	   z0 = svld1rq_u32 (p0, x0),
	   z0 = svld1rq (p0, x0))

/*
** ld1rq_u32_index:
**	ld1rqw	z0\.s, p0/z, \[x0, x1, lsl 2\]
**	ret
*/
TEST_LOAD (ld1rq_u32_index, svuint32_t, uint32_t,
	   z0 = svld1rq_u32 (p0, x0 + x1),
	   z0 = svld1rq (p0, x0 + x1))

/*
** ld1rq_u32_1:
**	add	(x[0-9]+), x0, #?4
**	ld1rqw	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u32_1, svuint32_t, uint32_t,
	   z0 = svld1rq_u32 (p0, x0 + 1),
	   z0 = svld1rq (p0, x0 + 1))

/*
** ld1rq_u32_2:
**	add	(x[0-9]+), x0, #?8
**	ld1rqw	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u32_2, svuint32_t, uint32_t,
	   z0 = svld1rq_u32 (p0, x0 + 2),
	   z0 = svld1rq (p0, x0 + 2))

/*
** ld1rq_u32_3:
**	add	(x[0-9]+), x0, #?12
**	ld1rqw	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u32_3, svuint32_t, uint32_t,
	   z0 = svld1rq_u32 (p0, x0 + 3),
	   z0 = svld1rq (p0, x0 + 3))

/*
** ld1rq_u32_4:
**	ld1rqw	z0\.s, p0/z, \[x0, #?16\]
**	ret
*/
TEST_LOAD (ld1rq_u32_4, svuint32_t, uint32_t,
	   z0 = svld1rq_u32 (p0, x0 + 4),
	   z0 = svld1rq (p0, x0 + 4))

/*
** ld1rq_u32_28:
**	ld1rqw	z0\.s, p0/z, \[x0, #?112\]
**	ret
*/
TEST_LOAD (ld1rq_u32_28, svuint32_t, uint32_t,
	   z0 = svld1rq_u32 (p0, x0 + 28),
	   z0 = svld1rq (p0, x0 + 28))

/*
** ld1rq_u32_32:
**	add	(x[0-9]+), x0, #?128
**	ld1rqw	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u32_32, svuint32_t, uint32_t,
	   z0 = svld1rq_u32 (p0, x0 + 32),
	   z0 = svld1rq (p0, x0 + 32))

/*
** ld1rq_u32_m1:
**	sub	(x[0-9]+), x0, #?4
**	ld1rqw	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u32_m1, svuint32_t, uint32_t,
	   z0 = svld1rq_u32 (p0, x0 - 1),
	   z0 = svld1rq (p0, x0 - 1))

/*
** ld1rq_u32_m2:
**	sub	(x[0-9]+), x0, #?8
**	ld1rqw	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u32_m2, svuint32_t, uint32_t,
	   z0 = svld1rq_u32 (p0, x0 - 2),
	   z0 = svld1rq (p0, x0 - 2))

/*
** ld1rq_u32_m3:
**	sub	(x[0-9]+), x0, #?12
**	ld1rqw	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u32_m3, svuint32_t, uint32_t,
	   z0 = svld1rq_u32 (p0, x0 - 3),
	   z0 = svld1rq (p0, x0 - 3))

/*
** ld1rq_u32_m4:
**	ld1rqw	z0\.s, p0/z, \[x0, #?-16\]
**	ret
*/
TEST_LOAD (ld1rq_u32_m4, svuint32_t, uint32_t,
	   z0 = svld1rq_u32 (p0, x0 - 4),
	   z0 = svld1rq (p0, x0 - 4))

/*
** ld1rq_u32_m32:
**	ld1rqw	z0\.s, p0/z, \[x0, #?-128\]
**	ret
*/
TEST_LOAD (ld1rq_u32_m32, svuint32_t, uint32_t,
	   z0 = svld1rq_u32 (p0, x0 - 32),
	   z0 = svld1rq (p0, x0 - 32))

/*
** ld1rq_u32_m36:
**	sub	(x[0-9]+), x0, #?144
**	ld1rqw	z0\.s, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1rq_u32_m36, svuint32_t, uint32_t,
	   z0 = svld1rq_u32 (p0, x0 - 36),
	   z0 = svld1rq (p0, x0 - 36))
