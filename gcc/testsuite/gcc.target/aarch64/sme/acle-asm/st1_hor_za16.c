/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** st1_za16_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	st1h	{ za0h\.h\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za16_0_0,
	       svst1_hor_za16 (0, 0, p0, x1),
	       svst1_hor_za16 (0, 0, p0, x1))

/* It would also be OK (and perhaps better) to move 0 into a register
   and use an offset of 7.  */
/*
** st1_za16_0_7:
**	mov	(w1[2-5]), #?7
**	st1h	{ za0h\.h\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za16_0_7,
	       svst1_hor_za16 (0, 7, p0, x1),
	       svst1_hor_za16 (0, 7, p0, x1))

/*
** st1_za16_0_8:
**	mov	(w1[2-5]), #?8
**	st1h	{ za0h\.h\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za16_0_8,
	       svst1_hor_za16 (0, 8, p0, x1),
	       svst1_hor_za16 (0, 8, p0, x1))

/*
** st1_za16_0_w0:
**	mov	(w1[2-5]), w0
**	st1h	{ za0h\.h\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za16_0_w0,
	       svst1_hor_za16 (0, w0, p0, x1),
	       svst1_hor_za16 (0, w0, p0, x1))

/*
** st1_za16_0_w0_p1:
**	mov	(w1[2-5]), w0
**	st1h	{ za0h\.h\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za16_0_w0_p1,
	       svst1_hor_za16 (0, w0 + 1, p0, x1),
	       svst1_hor_za16 (0, w0 + 1, p0, x1))

/*
** st1_za16_0_w0_p7:
**	mov	(w1[2-5]), w0
**	st1h	{ za0h\.h\[\1, 7\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za16_0_w0_p7,
	       svst1_hor_za16 (0, w0 + 7, p0, x1),
	       svst1_hor_za16 (0, w0 + 7, p0, x1))

/*
** st1_za16_1_w0:
**	mov	(w1[2-5]), w0
**	st1h	{ za1h\.h\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za16_1_w0,
	       svst1_hor_za16 (1, w0, p0, x1),
	       svst1_hor_za16 (1, w0, p0, x1))


/*
** st1_za16_1_w0_p1:
**	mov	(w1[2-5]), w0
**	st1h	{ za1h\.h\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za16_1_w0_p1,
	       svst1_hor_za16 (1, w0 + 1, p0, x1),
	       svst1_hor_za16 (1, w0 + 1, p0, x1))

/*
** st1_za16_1_w0_p7:
**	mov	(w1[2-5]), w0
**	st1h	{ za1h\.h\[\1, 7\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za16_1_w0_p7,
	       svst1_hor_za16 (1, w0 + 7, p0, x1),
	       svst1_hor_za16 (1, w0 + 7, p0, x1))

/*
** st1_za16_1_w0_p5_index:
**	mov	(w1[2-5]), w0
**	st1h	{ za1h\.h\[\1, 5\] }, p0, \[x1, x2, lsl #?1\]
**	ret
*/
TEST_STORE_ZA (st1_za16_1_w0_p5_index,
	       svst1_hor_za16 (1, w0 + 5, p0, x1 + x2 * 2),
	       svst1_hor_za16 (1, w0 + 5, p0, x1 + x2 * 2))

/*
** st1_za16_0_w0_p8:
**	add	(w1[2-5]), w0, #?8
**	st1h	{ za0h\.h\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za16_0_w0_p8,
	       svst1_hor_za16 (0, w0 + 8, p0, x1),
	       svst1_hor_za16 (0, w0 + 8, p0, x1))

/*
** st1_za16_0_w0_m1:
**	sub	(w1[2-5]), w0, #?1
**	st1h	{ za0h\.h\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za16_0_w0_m1,
	       svst1_hor_za16 (0, w0 - 1, p0, x1),
	       svst1_hor_za16 (0, w0 - 1, p0, x1))
