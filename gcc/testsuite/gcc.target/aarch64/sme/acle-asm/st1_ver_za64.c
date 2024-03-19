/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** st1_za64_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	st1d	{ za0v\.d\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za64_0_0,
	       svst1_ver_za64 (0, 0, p0, x1),
	       svst1_ver_za64 (0, 0, p0, x1))

/* It would also be OK (and perhaps better) to move 0 into a register
   and use an offset of 1.  */
/*
** st1_za64_0_1:
**	mov	(w1[2-5]), #?1
**	st1d	{ za0v\.d\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za64_0_1,
	       svst1_ver_za64 (0, 1, p0, x1),
	       svst1_ver_za64 (0, 1, p0, x1))

/*
** st1_za64_0_2:
**	mov	(w1[2-5]), #?2
**	st1d	{ za0v\.d\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za64_0_2,
	       svst1_ver_za64 (0, 2, p0, x1),
	       svst1_ver_za64 (0, 2, p0, x1))

/*
** st1_za64_0_w0:
**	mov	(w1[2-5]), w0
**	st1d	{ za0v\.d\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za64_0_w0,
	       svst1_ver_za64 (0, w0, p0, x1),
	       svst1_ver_za64 (0, w0, p0, x1))

/*
** st1_za64_0_w0_p1:
**	mov	(w1[2-5]), w0
**	st1d	{ za0v\.d\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za64_0_w0_p1,
	       svst1_ver_za64 (0, w0 + 1, p0, x1),
	       svst1_ver_za64 (0, w0 + 1, p0, x1))

/*
** st1_za64_7_w0:
**	mov	(w1[2-5]), w0
**	st1d	{ za7v\.d\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za64_7_w0,
	       svst1_ver_za64 (7, w0, p0, x1),
	       svst1_ver_za64 (7, w0, p0, x1))

/*
** st1_za64_7_w0_p1:
**	mov	(w1[2-5]), w0
**	st1d	{ za7v\.d\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za64_7_w0_p1,
	       svst1_ver_za64 (7, w0 + 1, p0, x1),
	       svst1_ver_za64 (7, w0 + 1, p0, x1))

/*
** st1_za64_5_w0_p1_index:
**	mov	(w1[2-5]), w0
**	st1d	{ za5v\.d\[\1, 1\] }, p0, \[x1, x2, lsl #?3\]
**	ret
*/
TEST_STORE_ZA (st1_za64_5_w0_p1_index,
	       svst1_ver_za64 (5, w0 + 1, p0, x1 + x2 * 8),
	       svst1_ver_za64 (5, w0 + 1, p0, x1 + x2 * 8))

/*
** st1_za64_0_w0_p2:
**	add	(w1[2-5]), w0, #?2
**	st1d	{ za0v\.d\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za64_0_w0_p2,
	       svst1_ver_za64 (0, w0 + 2, p0, x1),
	       svst1_ver_za64 (0, w0 + 2, p0, x1))

/*
** st1_za64_0_w0_m1:
**	sub	(w1[2-5]), w0, #?1
**	st1d	{ za0v\.d\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za64_0_w0_m1,
	       svst1_ver_za64 (0, w0 - 1, p0, x1),
	       svst1_ver_za64 (0, w0 - 1, p0, x1))
