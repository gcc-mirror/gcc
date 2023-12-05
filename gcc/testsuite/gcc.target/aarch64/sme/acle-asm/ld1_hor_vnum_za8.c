/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** ld1_vnum_za8_0_0_1:
**	incb	x1
**	mov	(w1[2-5]), (?:wzr|#?0)
**	ld1b	{ za0h\.b\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za8_0_0_1,
	      svld1_hor_vnum_za8 (0, 0, p0, x1, 1),
	      svld1_hor_vnum_za8 (0, 0, p0, x1, 1))

/*
** ld1_vnum_za8_0_1_1:
**	incb	x1
**	mov	(w1[2-5]), #?1
**	ld1b	{ za0h\.b\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za8_0_1_1,
	      svld1_hor_vnum_za8 (0, 1, p0, x1, 1),
	      svld1_hor_vnum_za8 (0, 1, p0, x1, 1))

/*
** ld1_vnum_za8_0_0_16:
**	incb	x1, all, mul #16
**	mov	(w1[2-5]), #?16
**	ld1b	{ za0h\.b\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za8_0_0_16,
	      svld1_hor_vnum_za8 (0, 0, p0, x1, 16),
	      svld1_hor_vnum_za8 (0, 0, p0, x1, 16))

/*
** ld1_vnum_za8_0_1_16:
**	incb	x1, all, mul #16
**	mov	(w1[2-5]), #?17
**	ld1b	{ za0h\.b\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za8_0_1_16,
	      svld1_hor_vnum_za8 (0, 1, p0, x1, 16),
	      svld1_hor_vnum_za8 (0, 1, p0, x1, 16))

/*
** ld1_vnum_za8_0_w0_0:
**	mov	(w1[2-5]), w0
**	ld1b	{ za0h\.b\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za8_0_w0_0,
	      svld1_hor_vnum_za8 (0, w0, p0, x1, 0),
	      svld1_hor_vnum_za8 (0, w0, p0, x1, 0))

/*
** ld1_vnum_za8_0_w0_1:
**	incb	x1
**	mov	(w1[2-5]), w0
**	ld1b	{ za0h\.b\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za8_0_w0_1,
	      svld1_hor_vnum_za8 (0, w0, p0, x1, 1),
	      svld1_hor_vnum_za8 (0, w0, p0, x1, 1))

/*
** ld1_vnum_za8_0_w0_15:
**	incb	x1, all, mul #15
**	mov	(w1[2-5]), w0
**	ld1b	{ za0h\.b\[\1, 15\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za8_0_w0_15,
	      svld1_hor_vnum_za8 (0, w0, p0, x1, 15),
	      svld1_hor_vnum_za8 (0, w0, p0, x1, 15))

/*
** ld1_vnum_za8_0_w0_16:
**	incb	x1, all, mul #16
**	add	(w1[2-5]), w0, #?16
**	ld1b	{ za0h\.b\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za8_0_w0_16,
	      svld1_hor_vnum_za8 (0, w0, p0, x1, 16),
	      svld1_hor_vnum_za8 (0, w0, p0, x1, 16))

/*
** ld1_vnum_za8_0_w0_x2:
**	cntb	(x[0-9]+)
**	mul	(x[0-9]+), (?:\1, x2|x2, \1)
**	add	(w1[2-5]), (?:w0, w2|w2, w0)
**	ld1b	{ za0h\.b\[\3, 0\] }, p0/z, \[x1, \2\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za8_0_w0_x2,
	      svld1_hor_vnum_za8 (0, w0, p0, x1, x2),
	      svld1_hor_vnum_za8 (0, w0, p0, x1, x2))

/*
** ld1_vnum_za8_0_w0p1_0:
**	mov	(w1[2-5]), w0
**	ld1b	{ za0h\.b\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za8_0_w0p1_0,
	      svld1_hor_vnum_za8 (0, w0 + 1, p0, x1, 0),
	      svld1_hor_vnum_za8 (0, w0 + 1, p0, x1, 0))
