/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** ld1_za128_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	ld1q	{ za0v\.q\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za128_0_0,
	      svld1_ver_za128 (0, 0, p0, x1),
	      svld1_ver_za128 (0, 0, p0, x1))

/*
** ld1_za128_0_1:
**	mov	(w1[2-5]), #?1
**	ld1q	{ za0v\.q\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za128_0_1,
	      svld1_ver_za128 (0, 1, p0, x1),
	      svld1_ver_za128 (0, 1, p0, x1))

/*
** ld1_za128_0_w0:
**	mov	(w1[2-5]), w0
**	ld1q	{ za0v\.q\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za128_0_w0,
	      svld1_ver_za128 (0, w0, p0, x1),
	      svld1_ver_za128 (0, w0, p0, x1))

/*
** ld1_za128_0_w0_p1:
**	add	(w1[2-5]), w0, #?1
**	ld1q	{ za0v\.q\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za128_0_w0_p1,
	      svld1_ver_za128 (0, w0 + 1, p0, x1),
	      svld1_ver_za128 (0, w0 + 1, p0, x1))

/*
** ld1_za128_7_w0:
**	mov	(w1[2-5]), w0
**	ld1q	{ za7v\.q\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za128_7_w0,
	      svld1_ver_za128 (7, w0, p0, x1),
	      svld1_ver_za128 (7, w0, p0, x1))

/*
** ld1_za128_13_w0:
**	mov	(w1[2-5]), w0
**	ld1q	{ za13v\.q\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za128_13_w0,
	      svld1_ver_za128 (13, w0, p0, x1),
	      svld1_ver_za128 (13, w0, p0, x1))

/*
** ld1_za128_15_w0:
**	mov	(w1[2-5]), w0
**	ld1q	{ za15v\.q\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za128_15_w0,
	      svld1_ver_za128 (15, w0, p0, x1),
	      svld1_ver_za128 (15, w0, p0, x1))

/*
** ld1_za128_9_w0_index:
**	mov	(w1[2-5]), w0
**	ld1q	{ za9v\.q\[\1, 0\] }, p0/z, \[x1, x2, lsl #?4\]
**	ret
*/
TEST_LOAD_ZA (ld1_za128_9_w0_index,
	      svld1_ver_za128 (9, w0, p0, x1 + x2 * 16),
	      svld1_ver_za128 (9, w0, p0, x1 + x2 * 16))
