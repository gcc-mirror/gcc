/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** st1_za128_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	st1q	{ za0v\.q\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za128_0_0,
	       svst1_ver_za128 (0, 0, p0, x1),
	       svst1_ver_za128 (0, 0, p0, x1))

/*
** st1_za128_0_1:
**	mov	(w1[2-5]), #?1
**	st1q	{ za0v\.q\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za128_0_1,
	       svst1_ver_za128 (0, 1, p0, x1),
	       svst1_ver_za128 (0, 1, p0, x1))

/*
** st1_za128_0_w0:
**	mov	(w1[2-5]), w0
**	st1q	{ za0v\.q\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za128_0_w0,
	       svst1_ver_za128 (0, w0, p0, x1),
	       svst1_ver_za128 (0, w0, p0, x1))

/*
** st1_za128_0_w0_p1:
**	add	(w1[2-5]), w0, #?1
**	st1q	{ za0v\.q\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za128_0_w0_p1,
	       svst1_ver_za128 (0, w0 + 1, p0, x1),
	       svst1_ver_za128 (0, w0 + 1, p0, x1))

/*
** st1_za128_7_w0:
**	mov	(w1[2-5]), w0
**	st1q	{ za7v\.q\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za128_7_w0,
	       svst1_ver_za128 (7, w0, p0, x1),
	       svst1_ver_za128 (7, w0, p0, x1))

/*
** st1_za128_13_w0:
**	mov	(w1[2-5]), w0
**	st1q	{ za13v\.q\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za128_13_w0,
	       svst1_ver_za128 (13, w0, p0, x1),
	       svst1_ver_za128 (13, w0, p0, x1))

/*
** st1_za128_15_w0:
**	mov	(w1[2-5]), w0
**	st1q	{ za15v\.q\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za128_15_w0,
	       svst1_ver_za128 (15, w0, p0, x1),
	       svst1_ver_za128 (15, w0, p0, x1))

/*
** st1_za128_9_w0_index:
**	mov	(w1[2-5]), w0
**	st1q	{ za9v\.q\[\1, 0\] }, p0, \[x1, x2, lsl #?4\]
**	ret
*/
TEST_STORE_ZA (st1_za128_9_w0_index,
	       svst1_ver_za128 (9, w0, p0, x1 + x2 * 16),
	       svst1_ver_za128 (9, w0, p0, x1 + x2 * 16))
