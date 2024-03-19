/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** ld1_vnum_za128_0_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	ld1q	{ za0h\.q\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za128_0_0_0,
	      svld1_hor_vnum_za128 (0, 0, p0, x1, 0),
	      svld1_hor_vnum_za128 (0, 0, p0, x1, 0))

/*
** ld1_vnum_za128_7_1_0:
**	mov	(w1[2-5]), #?1
**	ld1q	{ za7h\.q\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za128_7_1_0,
	      svld1_hor_vnum_za128 (7, 1, p0, x1, 0),
	      svld1_hor_vnum_za128 (7, 1, p0, x1, 0))

/*
** ld1_vnum_za128_11_1_5:
**	incb	x1, all, mul #5
**	mov	(w1[2-5]), #?6
**	ld1q	{ za11h\.q\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za128_11_1_5,
	      svld1_hor_vnum_za128 (11, 1, p0, x1, 5),
	      svld1_hor_vnum_za128 (11, 1, p0, x1, 5))

/*
** ld1_vnum_za128_3_w0_0:
**	mov	(w1[2-5]), w0
**	ld1q	{ za3h\.q\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za128_3_w0_0,
	      svld1_hor_vnum_za128 (3, w0, p0, x1, 0),
	      svld1_hor_vnum_za128 (3, w0, p0, x1, 0))

/*
** ld1_vnum_za128_5_w0_0:
**	incb	x1, all, mul #13
**	add	(w1[2-5]), w0, #?13
**	ld1q	{ za5h\.q\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za128_5_w0_0,
	      svld1_hor_vnum_za128 (5, w0, p0, x1, 13),
	      svld1_hor_vnum_za128 (5, w0, p0, x1, 13))

/*
** ld1_vnum_za128_11_w0_0:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (?:\1, x2|x2, \1), x1
**	add	(w1[2-5]), (?:w0, w2|w2, w0)
**	ld1q	{ za11h\.q\[\3, 0\] }, p0/z, \[\2\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za128_11_w0_0,
	      svld1_hor_vnum_za128 (11, w0, p0, x1, x2),
	      svld1_hor_vnum_za128 (11, w0, p0, x1, x2))

/*
** ld1_vnum_za128_15_w0p1_0:
**	add	(w1[2-5]), w0, #?1
**	ld1q	{ za15h\.q\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za128_15_w0p1_0,
	      svld1_hor_vnum_za128 (15, w0 + 1, p0, x1, 0),
	      svld1_hor_vnum_za128 (15, w0 + 1, p0, x1, 0))
