/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** st1_za8_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	st1b	{ za0v\.b\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za8_0_0,
	       svst1_ver_za8 (0, 0, p0, x1),
	       svst1_ver_za8 (0, 0, p0, x1))

/* It would also be OK (and perhaps better) to move 0 into a register
   and use an offset of 15.  */
/*
** st1_za8_0_15:
**	mov	(w1[2-5]), #?15
**	st1b	{ za0v\.b\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za8_0_15,
	       svst1_ver_za8 (0, 15, p0, x1),
	       svst1_ver_za8 (0, 15, p0, x1))

/*
** st1_za8_0_16:
**	mov	(w1[2-5]), #?16
**	st1b	{ za0v\.b\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za8_0_16,
	       svst1_ver_za8 (0, 16, p0, x1),
	       svst1_ver_za8 (0, 16, p0, x1))

/*
** st1_za8_0_w0:
**	mov	(w1[2-5]), w0
**	st1b	{ za0v\.b\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za8_0_w0,
	       svst1_ver_za8 (0, w0, p0, x1),
	       svst1_ver_za8 (0, w0, p0, x1))

/*
** st1_za8_0_w0_p1:
**	mov	(w1[2-5]), w0
**	st1b	{ za0v\.b\[\1, 1\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za8_0_w0_p1,
	       svst1_ver_za8 (0, w0 + 1, p0, x1),
	       svst1_ver_za8 (0, w0 + 1, p0, x1))

/*
** st1_za8_0_w0_p15:
**	mov	(w1[2-5]), w0
**	st1b	{ za0v\.b\[\1, 15\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za8_0_w0_p15,
	       svst1_ver_za8 (0, w0 + 15, p0, x1),
	       svst1_ver_za8 (0, w0 + 15, p0, x1))

/*
** st1_za8_0_w0_p13_index:
**	mov	(w1[2-5]), w0
**	st1b	{ za0v\.b\[\1, 15\] }, p0, \[x1, x2\]
**	ret
*/
TEST_STORE_ZA (st1_za8_0_w0_p13_index,
	       svst1_ver_za8 (0, w0 + 15, p0, x1 + x2),
	       svst1_ver_za8 (0, w0 + 15, p0, x1 + x2))

/*
** st1_za8_0_w0_p16:
**	add	(w1[2-5]), w0, #?16
**	st1b	{ za0v\.b\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za8_0_w0_p16,
	       svst1_ver_za8 (0, w0 + 16, p0, x1),
	       svst1_ver_za8 (0, w0 + 16, p0, x1))

/*
** st1_za8_0_w0_m1:
**	sub	(w1[2-5]), w0, #?1
**	st1b	{ za0v\.b\[\1, 0\] }, p0, \[x1\]
**	ret
*/
TEST_STORE_ZA (st1_za8_0_w0_m1,
	       svst1_ver_za8 (0, w0 - 1, p0, x1),
	       svst1_ver_za8 (0, w0 - 1, p0, x1))
