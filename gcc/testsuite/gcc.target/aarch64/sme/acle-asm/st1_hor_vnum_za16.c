/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** st1_vnum_za16_1_0_1:
**	incb	x1
**	mov	(w1[2-5]), (?:wzr|#?0)
**	st1h	{ za1h\.h\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za16_1_0_1,
	       svst1_hor_vnum_za16 (1, 0, p0, x1, 1),
	       svst1_hor_vnum_za16 (1, 0, p0, x1, 1))

/*
** st1_vnum_za16_1_1_1:
**	incb	x1
**	mov	(w1[2-5]), #?1
**	st1h	{ za1h\.h\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za16_1_1_1,
	       svst1_hor_vnum_za16 (1, 1, p0, x1, 1),
	       svst1_hor_vnum_za16 (1, 1, p0, x1, 1))

/*
** st1_vnum_za16_0_0_8:
**	incb	x1, all, mul #8
**	mov	(w1[2-5]), #?8
**	st1h	{ za0h\.h\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za16_0_0_8,
	       svst1_hor_vnum_za16 (0, 0, p0, x1, 8),
	       svst1_hor_vnum_za16 (0, 0, p0, x1, 8))

/*
** st1_vnum_za16_0_1_8:
**	incb	x1, all, mul #8
**	mov	(w1[2-5]), #?9
**	st1h	{ za0h\.h\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za16_0_1_8,
	       svst1_hor_vnum_za16 (0, 1, p0, x1, 8),
	       svst1_hor_vnum_za16 (0, 1, p0, x1, 8))

/*
** st1_vnum_za16_0_w0_0:
**	mov	(w1[2-5]), w0
**	st1h	{ za0h\.h\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za16_0_w0_0,
	       svst1_hor_vnum_za16 (0, w0, p0, x1, 0),
	       svst1_hor_vnum_za16 (0, w0, p0, x1, 0))

/*
** st1_vnum_za16_0_w0_1:
**	incb	x1
**	mov	(w1[2-5]), w0
**	st1h	{ za0h\.h\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za16_0_w0_1,
	       svst1_hor_vnum_za16 (0, w0, p0, x1, 1),
	       svst1_hor_vnum_za16 (0, w0, p0, x1, 1))

/*
** st1_vnum_za16_0_w0_7:
**	incb	x1, all, mul #7
**	mov	(w1[2-5]), w0
**	st1h	{ za0h\.h\[\1, 7\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za16_0_w0_7,
	       svst1_hor_vnum_za16 (0, w0, p0, x1, 7),
	       svst1_hor_vnum_za16 (0, w0, p0, x1, 7))

/*
** st1_vnum_za16_1_w0_8:
**	incb	x1, all, mul #8
**	add	(w1[2-5]), w0, #?8
**	st1h	{ za1h\.h\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za16_1_w0_8,
	       svst1_hor_vnum_za16 (1, w0, p0, x1, 8),
	       svst1_hor_vnum_za16 (1, w0, p0, x1, 8))

/*
** st1_vnum_za16_1_w0_13:
**	incb	x1, all, mul #13
**	add	(w1[2-5]), w0, #?13
**	st1h	{ za1h\.h\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za16_1_w0_13,
	       svst1_hor_vnum_za16 (1, w0, p0, x1, 13),
	       svst1_hor_vnum_za16 (1, w0, p0, x1, 13))

/*
** st1_vnum_za16_0_w0_x2:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (?:\1, x2|x2, \1), x1
**	add	(w1[2-5]), (?:w0, w2|w2, w0)
**	st1h	{ za0h\.h\[\3, 0\] }, p0, \[\2\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za16_0_w0_x2,
	       svst1_hor_vnum_za16 (0, w0, p0, x1, x2),
	       svst1_hor_vnum_za16 (0, w0, p0, x1, x2))

/*
** st1_vnum_za16_1_w0p1_0:
**	mov	(w1[2-5]), w0
**	st1h	{ za1h\.h\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za16_1_w0p1_0,
	       svst1_hor_vnum_za16 (1, w0 + 1, p0, x1, 0),
	       svst1_hor_vnum_za16 (1, w0 + 1, p0, x1, 0))
