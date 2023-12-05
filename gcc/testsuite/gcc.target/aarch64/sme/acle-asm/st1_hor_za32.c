/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** st1_za32_0_0:
**	mov	(w1[2-5]), (?:w0|#?0)
**	st1w	{ za0h\.s\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za32_0_0,
	       svst1_hor_za32 (0, 0, p0, x1),
	       svst1_hor_za32 (0, 0, p0, x1))

/* It would also be OK (and perhaps better) to move 0 into a register
   and use an offset of 3.  */
/*
** st1_za32_0_3:
**	mov	(w1[2-5]), #?3
**	st1w	{ za0h\.s\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za32_0_3,
	       svst1_hor_za32 (0, 3, p0, x1),
	       svst1_hor_za32 (0, 3, p0, x1))

/*
** st1_za32_0_4:
**	mov	(w1[2-5]), #?4
**	st1w	{ za0h\.s\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za32_0_4,
	       svst1_hor_za32 (0, 4, p0, x1),
	       svst1_hor_za32 (0, 4, p0, x1))

/*
** st1_za32_0_w0:
**	mov	(w1[2-5]), w0
**	st1w	{ za0h\.s\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za32_0_w0,
	       svst1_hor_za32 (0, w0, p0, x1),
	       svst1_hor_za32 (0, w0, p0, x1))

/*
** st1_za32_0_w0_p1:
**	mov	(w1[2-5]), w0
**	st1w	{ za0h\.s\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za32_0_w0_p1,
	       svst1_hor_za32 (0, w0 + 1, p0, x1),
	       svst1_hor_za32 (0, w0 + 1, p0, x1))

/*
** st1_za32_0_w0_p3:
**	mov	(w1[2-5]), w0
**	st1w	{ za0h\.s\[\1, 3\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za32_0_w0_p3,
	       svst1_hor_za32 (0, w0 + 3, p0, x1),
	       svst1_hor_za32 (0, w0 + 3, p0, x1))

/*
** st1_za32_3_w0:
**	mov	(w1[2-5]), w0
**	st1w	{ za3h\.s\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za32_3_w0,
	       svst1_hor_za32 (3, w0, p0, x1),
	       svst1_hor_za32 (3, w0, p0, x1))

/*
** st1_za32_3_w0_p1:
**	mov	(w1[2-5]), w0
**	st1w	{ za3h\.s\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za32_3_w0_p1,
	       svst1_hor_za32 (3, w0 + 1, p0, x1),
	       svst1_hor_za32 (3, w0 + 1, p0, x1))

/*
** st1_za32_3_w0_p3:
**	mov	(w1[2-5]), w0
**	st1w	{ za3h\.s\[\1, 3\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za32_3_w0_p3,
	       svst1_hor_za32 (3, w0 + 3, p0, x1),
	       svst1_hor_za32 (3, w0 + 3, p0, x1))

/*
** st1_za32_1_w0_p2_index:
**	mov	(w1[2-5]), w0
**	st1w	{ za1h\.s\[\1, 2\] }, p0, \[x1, x2, lsl #?2\]
**	ret
*/
TEST_STORE_ZA (st1_za32_1_w0_p2_index,
	       svst1_hor_za32 (1, w0 + 2, p0, x1 + x2 * 4),
	       svst1_hor_za32 (1, w0 + 2, p0, x1 + x2 * 4))

/*
** st1_za32_0_w0_p4:
**	add	(w1[2-5]), w0, #?4
**	st1w	{ za0h\.s\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za32_0_w0_p4,
	       svst1_hor_za32 (0, w0 + 4, p0, x1),
	       svst1_hor_za32 (0, w0 + 4, p0, x1))

/*
** st1_za32_0_w0_m1:
**	sub	(w1[2-5]), w0, #?1
**	st1w	{ za0h\.s\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za32_0_w0_m1,
	       svst1_hor_za32 (0, w0 - 1, p0, x1),
	       svst1_hor_za32 (0, w0 - 1, p0, x1))
