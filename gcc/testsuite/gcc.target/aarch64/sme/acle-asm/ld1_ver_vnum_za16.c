/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** ld1_vnum_za16_1_0_1:
**	incb	x1
**	mov	(w1[2-5]), (?:wzr|#?0)
**	ld1h	{ za1v\.h\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za16_1_0_1,
	      svld1_ver_vnum_za16 (1, 0, p0, x1, 1),
	      svld1_ver_vnum_za16 (1, 0, p0, x1, 1))

/*
** ld1_vnum_za16_1_1_1:
**	incb	x1
**	mov	(w1[2-5]), #?1
**	ld1h	{ za1v\.h\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za16_1_1_1,
	      svld1_ver_vnum_za16 (1, 1, p0, x1, 1),
	      svld1_ver_vnum_za16 (1, 1, p0, x1, 1))

/*
** ld1_vnum_za16_0_0_8:
**	incb	x1, all, mul #8
**	mov	(w1[2-5]), #?8
**	ld1h	{ za0v\.h\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za16_0_0_8,
	      svld1_ver_vnum_za16 (0, 0, p0, x1, 8),
	      svld1_ver_vnum_za16 (0, 0, p0, x1, 8))

/*
** ld1_vnum_za16_0_1_8:
**	incb	x1, all, mul #8
**	mov	(w1[2-5]), #?9
**	ld1h	{ za0v\.h\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za16_0_1_8,
	      svld1_ver_vnum_za16 (0, 1, p0, x1, 8),
	      svld1_ver_vnum_za16 (0, 1, p0, x1, 8))

/*
** ld1_vnum_za16_0_w0_0:
**	mov	(w1[2-5]), w0
**	ld1h	{ za0v\.h\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za16_0_w0_0,
	      svld1_ver_vnum_za16 (0, w0, p0, x1, 0),
	      svld1_ver_vnum_za16 (0, w0, p0, x1, 0))

/*
** ld1_vnum_za16_0_w0_1:
**	incb	x1
**	mov	(w1[2-5]), w0
**	ld1h	{ za0v\.h\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za16_0_w0_1,
	      svld1_ver_vnum_za16 (0, w0, p0, x1, 1),
	      svld1_ver_vnum_za16 (0, w0, p0, x1, 1))

/*
** ld1_vnum_za16_0_w0_7:
**	incb	x1, all, mul #7
**	mov	(w1[2-5]), w0
**	ld1h	{ za0v\.h\[\1, 7\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za16_0_w0_7,
	      svld1_ver_vnum_za16 (0, w0, p0, x1, 7),
	      svld1_ver_vnum_za16 (0, w0, p0, x1, 7))

/*
** ld1_vnum_za16_1_w0_8:
**	incb	x1, all, mul #8
**	add	(w1[2-5]), w0, #?8
**	ld1h	{ za1v\.h\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za16_1_w0_8,
	      svld1_ver_vnum_za16 (1, w0, p0, x1, 8),
	      svld1_ver_vnum_za16 (1, w0, p0, x1, 8))

/*
** ld1_vnum_za16_1_w0_13:
**	incb	x1, all, mul #13
**	add	(w1[2-5]), w0, #?13
**	ld1h	{ za1v\.h\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za16_1_w0_13,
	      svld1_ver_vnum_za16 (1, w0, p0, x1, 13),
	      svld1_ver_vnum_za16 (1, w0, p0, x1, 13))

/*
** ld1_vnum_za16_0_w0_x2:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (?:\1, x2|x2, \1), x1
**	add	(w1[2-5]), (?:w0, w2|w2, w0)
**	ld1h	{ za0v\.h\[\3, 0\] }, p0/z, \[\2\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za16_0_w0_x2,
	      svld1_ver_vnum_za16 (0, w0, p0, x1, x2),
	      svld1_ver_vnum_za16 (0, w0, p0, x1, x2))

/*
** ld1_vnum_za16_1_w0p1_0:
**	mov	(w1[2-5]), w0
**	ld1h	{ za1v\.h\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za16_1_w0p1_0,
	      svld1_ver_vnum_za16 (1, w0 + 1, p0, x1, 0),
	      svld1_ver_vnum_za16 (1, w0 + 1, p0, x1, 0))
