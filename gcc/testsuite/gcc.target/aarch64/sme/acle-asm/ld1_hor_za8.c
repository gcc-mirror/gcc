/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** ld1_za8_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	ld1b	{ za0h\.b\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za8_0_0,
	      svld1_hor_za8 (0, 0, p0, x1),
	      svld1_hor_za8 (0, 0, p0, x1))

/* It would also be OK (and perhaps better) to move 0 into a register
   and use an offset of 15.  */
/*
** ld1_za8_0_15:
**	mov	(w1[2-5]), #?15
**	ld1b	{ za0h\.b\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za8_0_15,
	      svld1_hor_za8 (0, 15, p0, x1),
	      svld1_hor_za8 (0, 15, p0, x1))

/*
** ld1_za8_0_16:
**	mov	(w1[2-5]), #?16
**	ld1b	{ za0h\.b\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za8_0_16,
	      svld1_hor_za8 (0, 16, p0, x1),
	      svld1_hor_za8 (0, 16, p0, x1))

/*
** ld1_za8_0_w0:
**	mov	(w1[2-5]), w0
**	ld1b	{ za0h\.b\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za8_0_w0,
	      svld1_hor_za8 (0, w0, p0, x1),
	      svld1_hor_za8 (0, w0, p0, x1))

/*
** ld1_za8_0_w0_p1:
**	mov	(w1[2-5]), w0
**	ld1b	{ za0h\.b\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za8_0_w0_p1,
	      svld1_hor_za8 (0, w0 + 1, p0, x1),
	      svld1_hor_za8 (0, w0 + 1, p0, x1))

/*
** ld1_za8_0_w0_p15:
**	mov	(w1[2-5]), w0
**	ld1b	{ za0h\.b\[\1, 15\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za8_0_w0_p15,
	      svld1_hor_za8 (0, w0 + 15, p0, x1),
	      svld1_hor_za8 (0, w0 + 15, p0, x1))

/*
** ld1_za8_0_w0_p13_index:
**	mov	(w1[2-5]), w0
**	ld1b	{ za0h\.b\[\1, 15\] }, p0/z, \[x1, x2\]
**	ret
*/
TEST_LOAD_ZA (ld1_za8_0_w0_p13_index,
	      svld1_hor_za8 (0, w0 + 15, p0, x1 + x2),
	      svld1_hor_za8 (0, w0 + 15, p0, x1 + x2))

/*
** ld1_za8_0_w0_p16:
**	add	(w1[2-5]), w0, #?16
**	ld1b	{ za0h\.b\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za8_0_w0_p16,
	      svld1_hor_za8 (0, w0 + 16, p0, x1),
	      svld1_hor_za8 (0, w0 + 16, p0, x1))

/*
** ld1_za8_0_w0_m1:
**	sub	(w1[2-5]), w0, #?1
**	ld1b	{ za0h\.b\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za8_0_w0_m1,
	      svld1_hor_za8 (0, w0 - 1, p0, x1),
	      svld1_hor_za8 (0, w0 - 1, p0, x1))
