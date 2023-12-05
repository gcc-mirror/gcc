/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** ld1_vnum_za64_3_0_1:
**	incb	x1
**	mov	(w1[2-5]), (?:wzr|#?0)
**	ld1d	{ za3v\.d\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za64_3_0_1,
	      svld1_ver_vnum_za64 (3, 0, p0, x1, 1),
	      svld1_ver_vnum_za64 (3, 0, p0, x1, 1))

/*
** ld1_vnum_za64_7_1_1:
**	incb	x1
**	mov	(w1[2-5]), #?1
**	ld1d	{ za7v\.d\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za64_7_1_1,
	      svld1_ver_vnum_za64 (7, 1, p0, x1, 1),
	      svld1_ver_vnum_za64 (7, 1, p0, x1, 1))

/*
** ld1_vnum_za64_0_0_2:
**	incb	x1, all, mul #2
**	mov	(w1[2-5]), #?2
**	ld1d	{ za0v\.d\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za64_0_0_2,
	      svld1_ver_vnum_za64 (0, 0, p0, x1, 2),
	      svld1_ver_vnum_za64 (0, 0, p0, x1, 2))

/*
** ld1_vnum_za64_5_1_2:
**	incb	x1, all, mul #2
**	mov	(w1[2-5]), #?3
**	ld1d	{ za5v\.d\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za64_5_1_2,
	      svld1_ver_vnum_za64 (5, 1, p0, x1, 2),
	      svld1_ver_vnum_za64 (5, 1, p0, x1, 2))

/*
** ld1_vnum_za64_0_w0_0:
**	mov	(w1[2-5]), w0
**	ld1d	{ za0v\.d\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za64_0_w0_0,
	      svld1_ver_vnum_za64 (0, w0, p0, x1, 0),
	      svld1_ver_vnum_za64 (0, w0, p0, x1, 0))

/*
** ld1_vnum_za64_0_w0_1:
**	incb	x1
**	mov	(w1[2-5]), w0
**	ld1d	{ za0v\.d\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za64_0_w0_1,
	      svld1_ver_vnum_za64 (0, w0, p0, x1, 1),
	      svld1_ver_vnum_za64 (0, w0, p0, x1, 1))

/*
** ld1_vnum_za64_6_w0_2:
**	incb	x1, all, mul #2
**	add	(w1[2-5]), w0, #?2
**	ld1d	{ za6v\.d\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za64_6_w0_2,
	      svld1_ver_vnum_za64 (6, w0, p0, x1, 2),
	      svld1_ver_vnum_za64 (6, w0, p0, x1, 2))

/*
** ld1_vnum_za64_2_w0_13:
**	incb	x1, all, mul #13
**	add	(w1[2-5]), w0, #?13
**	ld1d	{ za2v\.d\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za64_2_w0_13,
	      svld1_ver_vnum_za64 (2, w0, p0, x1, 13),
	      svld1_ver_vnum_za64 (2, w0, p0, x1, 13))

/*
** ld1_vnum_za64_4_w0_x2:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (?:\1, x2|x2, \1), x1
**	add	(w1[2-5]), (?:w0, w2|w2, w0)
**	ld1d	{ za4v\.d\[\3, 0\] }, p0/z, \[\2\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za64_4_w0_x2,
	      svld1_ver_vnum_za64 (4, w0, p0, x1, x2),
	      svld1_ver_vnum_za64 (4, w0, p0, x1, x2))

/*
** ld1_vnum_za64_1_w0p1_0:
**	mov	(w1[2-5]), w0
**	ld1d	{ za1v\.d\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za64_1_w0p1_0,
	      svld1_ver_vnum_za64 (1, w0 + 1, p0, x1, 0),
	      svld1_ver_vnum_za64 (1, w0 + 1, p0, x1, 0))
