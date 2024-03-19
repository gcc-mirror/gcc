/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** ld1_za64_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	ld1d	{ za0v\.d\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za64_0_0,
	      svld1_ver_za64 (0, 0, p0, x1),
	      svld1_ver_za64 (0, 0, p0, x1))

/* It would also be OK (and perhaps better) to move 0 into a register
   and use an offset of 1.  */
/*
** ld1_za64_0_1:
**	mov	(w1[2-5]), #?1
**	ld1d	{ za0v\.d\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za64_0_1,
	      svld1_ver_za64 (0, 1, p0, x1),
	      svld1_ver_za64 (0, 1, p0, x1))

/*
** ld1_za64_0_2:
**	mov	(w1[2-5]), #?2
**	ld1d	{ za0v\.d\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za64_0_2,
	      svld1_ver_za64 (0, 2, p0, x1),
	      svld1_ver_za64 (0, 2, p0, x1))

/*
** ld1_za64_0_w0:
**	mov	(w1[2-5]), w0
**	ld1d	{ za0v\.d\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za64_0_w0,
	      svld1_ver_za64 (0, w0, p0, x1),
	      svld1_ver_za64 (0, w0, p0, x1))

/*
** ld1_za64_0_w0_p1:
**	mov	(w1[2-5]), w0
**	ld1d	{ za0v\.d\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za64_0_w0_p1,
	      svld1_ver_za64 (0, w0 + 1, p0, x1),
	      svld1_ver_za64 (0, w0 + 1, p0, x1))

/*
** ld1_za64_7_w0:
**	mov	(w1[2-5]), w0
**	ld1d	{ za7v\.d\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za64_7_w0,
	      svld1_ver_za64 (7, w0, p0, x1),
	      svld1_ver_za64 (7, w0, p0, x1))

/*
** ld1_za64_7_w0_p1:
**	mov	(w1[2-5]), w0
**	ld1d	{ za7v\.d\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za64_7_w0_p1,
	      svld1_ver_za64 (7, w0 + 1, p0, x1),
	      svld1_ver_za64 (7, w0 + 1, p0, x1))

/*
** ld1_za64_5_w0_p1_index:
**	mov	(w1[2-5]), w0
**	ld1d	{ za5v\.d\[\1, 1\] }, p0/z, \[x1, x2, lsl #?3\]
**	ret
*/
TEST_LOAD_ZA (ld1_za64_5_w0_p1_index,
	      svld1_ver_za64 (5, w0 + 1, p0, x1 + x2 * 8),
	      svld1_ver_za64 (5, w0 + 1, p0, x1 + x2 * 8))

/*
** ld1_za64_0_w0_p2:
**	add	(w1[2-5]), w0, #?2
**	ld1d	{ za0v\.d\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za64_0_w0_p2,
	      svld1_ver_za64 (0, w0 + 2, p0, x1),
	      svld1_ver_za64 (0, w0 + 2, p0, x1))

/*
** ld1_za64_0_w0_m1:
**	sub	(w1[2-5]), w0, #?1
**	ld1d	{ za0v\.d\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_za64_0_w0_m1,
	      svld1_ver_za64 (0, w0 - 1, p0, x1),
	      svld1_ver_za64 (0, w0 - 1, p0, x1))
