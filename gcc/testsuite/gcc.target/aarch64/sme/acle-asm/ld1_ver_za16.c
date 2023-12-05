/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** ld1_za16_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	ld1h	{ za0v\.h\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za16_0_0,
	      svld1_ver_za16 (0, 0, p0, x1),
	      svld1_ver_za16 (0, 0, p0, x1))

/* It would also be OK (and perhaps better) to move 0 into a register
   and use an offset of 7.  */
/*
** ld1_za16_0_7:
**	mov	(w1[2-5]), #?7
**	ld1h	{ za0v\.h\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za16_0_7,
	      svld1_ver_za16 (0, 7, p0, x1),
	      svld1_ver_za16 (0, 7, p0, x1))

/*
** ld1_za16_0_8:
**	mov	(w1[2-5]), #?8
**	ld1h	{ za0v\.h\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za16_0_8,
	      svld1_ver_za16 (0, 8, p0, x1),
	      svld1_ver_za16 (0, 8, p0, x1))

/*
** ld1_za16_0_w0:
**	mov	(w1[2-5]), w0
**	ld1h	{ za0v\.h\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za16_0_w0,
	      svld1_ver_za16 (0, w0, p0, x1),
	      svld1_ver_za16 (0, w0, p0, x1))

/*
** ld1_za16_0_w0_p1:
**	mov	(w1[2-5]), w0
**	ld1h	{ za0v\.h\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za16_0_w0_p1,
	      svld1_ver_za16 (0, w0 + 1, p0, x1),
	      svld1_ver_za16 (0, w0 + 1, p0, x1))

/*
** ld1_za16_0_w0_p7:
**	mov	(w1[2-5]), w0
**	ld1h	{ za0v\.h\[\1, 7\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za16_0_w0_p7,
	      svld1_ver_za16 (0, w0 + 7, p0, x1),
	      svld1_ver_za16 (0, w0 + 7, p0, x1))

/*
** ld1_za16_1_w0:
**	mov	(w1[2-5]), w0
**	ld1h	{ za1v\.h\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za16_1_w0,
	      svld1_ver_za16 (1, w0, p0, x1),
	      svld1_ver_za16 (1, w0, p0, x1))


/*
** ld1_za16_1_w0_p1:
**	mov	(w1[2-5]), w0
**	ld1h	{ za1v\.h\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za16_1_w0_p1,
	      svld1_ver_za16 (1, w0 + 1, p0, x1),
	      svld1_ver_za16 (1, w0 + 1, p0, x1))

/*
** ld1_za16_1_w0_p7:
**	mov	(w1[2-5]), w0
**	ld1h	{ za1v\.h\[\1, 7\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za16_1_w0_p7,
	      svld1_ver_za16 (1, w0 + 7, p0, x1),
	      svld1_ver_za16 (1, w0 + 7, p0, x1))

/*
** ld1_za16_1_w0_p5_index:
**	mov	(w1[2-5]), w0
**	ld1h	{ za1v\.h\[\1, 5\] }, p0/z, \[x1, x2, lsl #?1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za16_1_w0_p5_index,
	      svld1_ver_za16 (1, w0 + 5, p0, x1 + x2 * 2),
	      svld1_ver_za16 (1, w0 + 5, p0, x1 + x2 * 2))

/*
** ld1_za16_0_w0_p8:
**	add	(w1[2-5]), w0, #?8
**	ld1h	{ za0v\.h\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za16_0_w0_p8,
	      svld1_ver_za16 (0, w0 + 8, p0, x1),
	      svld1_ver_za16 (0, w0 + 8, p0, x1))

/*
** ld1_za16_0_w0_m1:
**	sub	(w1[2-5]), w0, #?1
**	ld1h	{ za0v\.h\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za16_0_w0_m1,
	      svld1_ver_za16 (0, w0 - 1, p0, x1),
	      svld1_ver_za16 (0, w0 - 1, p0, x1))
