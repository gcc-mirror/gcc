/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** st1_vnum_za8_0_0_1:
**	incb	x1
**	mov	(w1[2-5]), (?:wzr|#?0)
**	st1b	{ za0h\.b\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za8_0_0_1,
	       svst1_hor_vnum_za8 (0, 0, p0, x1, 1),
	       svst1_hor_vnum_za8 (0, 0, p0, x1, 1))

/*
** st1_vnum_za8_0_1_1:
**	incb	x1
**	mov	(w1[2-5]), #?1
**	st1b	{ za0h\.b\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za8_0_1_1,
	       svst1_hor_vnum_za8 (0, 1, p0, x1, 1),
	       svst1_hor_vnum_za8 (0, 1, p0, x1, 1))

/*
** st1_vnum_za8_0_0_16:
**	incb	x1, all, mul #16
**	mov	(w1[2-5]), #?16
**	st1b	{ za0h\.b\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za8_0_0_16,
	       svst1_hor_vnum_za8 (0, 0, p0, x1, 16),
	       svst1_hor_vnum_za8 (0, 0, p0, x1, 16))

/*
** st1_vnum_za8_0_1_16:
**	incb	x1, all, mul #16
**	mov	(w1[2-5]), #?17
**	st1b	{ za0h\.b\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za8_0_1_16,
	       svst1_hor_vnum_za8 (0, 1, p0, x1, 16),
	       svst1_hor_vnum_za8 (0, 1, p0, x1, 16))

/*
** st1_vnum_za8_0_w0_0:
**	mov	(w1[2-5]), w0
**	st1b	{ za0h\.b\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za8_0_w0_0,
	       svst1_hor_vnum_za8 (0, w0, p0, x1, 0),
	       svst1_hor_vnum_za8 (0, w0, p0, x1, 0))

/*
** st1_vnum_za8_0_w0_1:
**	incb	x1
**	mov	(w1[2-5]), w0
**	st1b	{ za0h\.b\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za8_0_w0_1,
	       svst1_hor_vnum_za8 (0, w0, p0, x1, 1),
	       svst1_hor_vnum_za8 (0, w0, p0, x1, 1))

/*
** st1_vnum_za8_0_w0_15:
**	incb	x1, all, mul #15
**	mov	(w1[2-5]), w0
**	st1b	{ za0h\.b\[\1, 15\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za8_0_w0_15,
	       svst1_hor_vnum_za8 (0, w0, p0, x1, 15),
	       svst1_hor_vnum_za8 (0, w0, p0, x1, 15))

/*
** st1_vnum_za8_0_w0_16:
**	incb	x1, all, mul #16
**	add	(w1[2-5]), w0, #?16
**	st1b	{ za0h\.b\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za8_0_w0_16,
	       svst1_hor_vnum_za8 (0, w0, p0, x1, 16),
	       svst1_hor_vnum_za8 (0, w0, p0, x1, 16))

/*
** st1_vnum_za8_0_w0_x2:
**	cntb	(x[0-9]+)
**	mul	(x[0-9]+), (?:\1, x2|x2, \1)
**	add	(w1[2-5]), (?:w0, w2|w2, w0)
**	st1b	{ za0h\.b\[\3, 0\] }, p0, \[x1, \2\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za8_0_w0_x2,
	       svst1_hor_vnum_za8 (0, w0, p0, x1, x2),
	       svst1_hor_vnum_za8 (0, w0, p0, x1, x2))

/*
** st1_vnum_za8_0_w0p1_0:
**	mov	(w1[2-5]), w0
**	st1b	{ za0h\.b\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za8_0_w0p1_0,
	       svst1_hor_vnum_za8 (0, w0 + 1, p0, x1, 0),
	       svst1_hor_vnum_za8 (0, w0 + 1, p0, x1, 0))
