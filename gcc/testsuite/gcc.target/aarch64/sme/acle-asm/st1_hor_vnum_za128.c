/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** st1_vnum_za128_0_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	st1q	{ za0h\.q\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za128_0_0_0,
	       svst1_hor_vnum_za128 (0, 0, p0, x1, 0),
	       svst1_hor_vnum_za128 (0, 0, p0, x1, 0))

/*
** st1_vnum_za128_7_1_0:
**	mov	(w1[2-5]), #?1
**	st1q	{ za7h\.q\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za128_7_1_0,
	       svst1_hor_vnum_za128 (7, 1, p0, x1, 0),
	       svst1_hor_vnum_za128 (7, 1, p0, x1, 0))

/*
** st1_vnum_za128_11_1_5:
**	incb	x1, all, mul #5
**	mov	(w1[2-5]), #?6
**	st1q	{ za11h\.q\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za128_11_1_5,
	       svst1_hor_vnum_za128 (11, 1, p0, x1, 5),
	       svst1_hor_vnum_za128 (11, 1, p0, x1, 5))

/*
** st1_vnum_za128_3_w0_0:
**	mov	(w1[2-5]), w0
**	st1q	{ za3h\.q\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za128_3_w0_0,
	       svst1_hor_vnum_za128 (3, w0, p0, x1, 0),
	       svst1_hor_vnum_za128 (3, w0, p0, x1, 0))

/*
** st1_vnum_za128_5_w0_0:
**	incb	x1, all, mul #13
**	add	(w1[2-5]), w0, #?13
**	st1q	{ za5h\.q\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za128_5_w0_0,
	       svst1_hor_vnum_za128 (5, w0, p0, x1, 13),
	       svst1_hor_vnum_za128 (5, w0, p0, x1, 13))

/*
** st1_vnum_za128_11_w0_0:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (?:\1, x2|x2, \1), x1
**	add	(w1[2-5]), (?:w0, w2|w2, w0)
**	st1q	{ za11h\.q\[\3, 0\] }, p0, \[\2\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za128_11_w0_0,
	       svst1_hor_vnum_za128 (11, w0, p0, x1, x2),
	       svst1_hor_vnum_za128 (11, w0, p0, x1, x2))

/*
** st1_vnum_za128_15_w0p1_0:
**	add	(w1[2-5]), w0, #?1
**	st1q	{ za15h\.q\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za128_15_w0p1_0,
	       svst1_hor_vnum_za128 (15, w0 + 1, p0, x1, 0),
	       svst1_hor_vnum_za128 (15, w0 + 1, p0, x1, 0))
