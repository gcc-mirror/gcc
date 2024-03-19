/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** st1_vnum_za32_3_0_1:
**	incb	x1
**	mov	(w1[2-5]), (?:wzr|#?0)
**	st1w	{ za3v\.s\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za32_3_0_1,
	       svst1_ver_vnum_za32 (3, 0, p0, x1, 1),
	       svst1_ver_vnum_za32 (3, 0, p0, x1, 1))

/*
** st1_vnum_za32_2_1_1:
**	incb	x1
**	mov	(w1[2-5]), #?1
**	st1w	{ za2v\.s\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za32_2_1_1,
	       svst1_ver_vnum_za32 (2, 1, p0, x1, 1),
	       svst1_ver_vnum_za32 (2, 1, p0, x1, 1))

/*
** st1_vnum_za32_0_0_4:
**	incb	x1, all, mul #4
**	mov	(w1[2-5]), #?4
**	st1w	{ za0v\.s\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za32_0_0_4,
	       svst1_ver_vnum_za32 (0, 0, p0, x1, 4),
	       svst1_ver_vnum_za32 (0, 0, p0, x1, 4))

/*
** st1_vnum_za32_2_1_4:
**	incb	x1, all, mul #4
**	mov	(w1[2-5]), #?5
**	st1w	{ za2v\.s\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za32_2_1_4,
	       svst1_ver_vnum_za32 (2, 1, p0, x1, 4),
	       svst1_ver_vnum_za32 (2, 1, p0, x1, 4))

/*
** st1_vnum_za32_0_w0_0:
**	mov	(w1[2-5]), w0
**	st1w	{ za0v\.s\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za32_0_w0_0,
	       svst1_ver_vnum_za32 (0, w0, p0, x1, 0),
	       svst1_ver_vnum_za32 (0, w0, p0, x1, 0))

/*
** st1_vnum_za32_0_w0_1:
**	incb	x1
**	mov	(w1[2-5]), w0
**	st1w	{ za0v\.s\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za32_0_w0_1,
	       svst1_ver_vnum_za32 (0, w0, p0, x1, 1),
	       svst1_ver_vnum_za32 (0, w0, p0, x1, 1))

/*
** st1_vnum_za32_0_w0_3:
**	incb	x1, all, mul #3
**	mov	(w1[2-5]), w0
**	st1w	{ za0v\.s\[\1, 3\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za32_0_w0_3,
	       svst1_ver_vnum_za32 (0, w0, p0, x1, 3),
	       svst1_ver_vnum_za32 (0, w0, p0, x1, 3))

/*
** st1_vnum_za32_1_w0_4:
**	incb	x1, all, mul #4
**	add	(w1[2-5]), w0, #?4
**	st1w	{ za1v\.s\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za32_1_w0_4,
	       svst1_ver_vnum_za32 (1, w0, p0, x1, 4),
	       svst1_ver_vnum_za32 (1, w0, p0, x1, 4))

/*
** st1_vnum_za32_3_w0_13:
**	incb	x1, all, mul #13
**	add	(w1[2-5]), w0, #?13
**	st1w	{ za3v\.s\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za32_3_w0_13,
	       svst1_ver_vnum_za32 (3, w0, p0, x1, 13),
	       svst1_ver_vnum_za32 (3, w0, p0, x1, 13))

/*
** st1_vnum_za32_0_w0_x2:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (?:\1, x2|x2, \1), x1
**	add	(w1[2-5]), (?:w0, w2|w2, w0)
**	st1w	{ za0v\.s\[\3, 0\] }, p0, \[\2\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za32_0_w0_x2,
	       svst1_ver_vnum_za32 (0, w0, p0, x1, x2),
	       svst1_ver_vnum_za32 (0, w0, p0, x1, x2))

/*
** st1_vnum_za32_1_w0p1_0:
**	mov	(w1[2-5]), w0
**	st1w	{ za1v\.s\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_vnum_za32_1_w0p1_0,
	       svst1_ver_vnum_za32 (1, w0 + 1, p0, x1, 0),
	       svst1_ver_vnum_za32 (1, w0 + 1, p0, x1, 0))
