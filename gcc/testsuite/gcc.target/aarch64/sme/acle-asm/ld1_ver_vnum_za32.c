/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** ld1_vnum_za32_3_0_1:
**	incb	x1
**	mov	(w1[2-5]), (?:wzr|#?0)
**	ld1w	{ za3v\.s\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za32_3_0_1,
	      svld1_ver_vnum_za32 (3, 0, p0, x1, 1),
	      svld1_ver_vnum_za32 (3, 0, p0, x1, 1))

/*
** ld1_vnum_za32_2_1_1:
**	incb	x1
**	mov	(w1[2-5]), #?1
**	ld1w	{ za2v\.s\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za32_2_1_1,
	      svld1_ver_vnum_za32 (2, 1, p0, x1, 1),
	      svld1_ver_vnum_za32 (2, 1, p0, x1, 1))

/*
** ld1_vnum_za32_0_0_4:
**	incb	x1, all, mul #4
**	mov	(w1[2-5]), #?4
**	ld1w	{ za0v\.s\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za32_0_0_4,
	      svld1_ver_vnum_za32 (0, 0, p0, x1, 4),
	      svld1_ver_vnum_za32 (0, 0, p0, x1, 4))

/*
** ld1_vnum_za32_2_1_4:
**	incb	x1, all, mul #4
**	mov	(w1[2-5]), #?5
**	ld1w	{ za2v\.s\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za32_2_1_4,
	      svld1_ver_vnum_za32 (2, 1, p0, x1, 4),
	      svld1_ver_vnum_za32 (2, 1, p0, x1, 4))

/*
** ld1_vnum_za32_0_w0_0:
**	mov	(w1[2-5]), w0
**	ld1w	{ za0v\.s\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za32_0_w0_0,
	      svld1_ver_vnum_za32 (0, w0, p0, x1, 0),
	      svld1_ver_vnum_za32 (0, w0, p0, x1, 0))

/*
** ld1_vnum_za32_0_w0_1:
**	incb	x1
**	mov	(w1[2-5]), w0
**	ld1w	{ za0v\.s\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za32_0_w0_1,
	      svld1_ver_vnum_za32 (0, w0, p0, x1, 1),
	      svld1_ver_vnum_za32 (0, w0, p0, x1, 1))

/*
** ld1_vnum_za32_0_w0_3:
**	incb	x1, all, mul #3
**	mov	(w1[2-5]), w0
**	ld1w	{ za0v\.s\[\1, 3\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za32_0_w0_3,
	      svld1_ver_vnum_za32 (0, w0, p0, x1, 3),
	      svld1_ver_vnum_za32 (0, w0, p0, x1, 3))

/*
** ld1_vnum_za32_1_w0_4:
**	incb	x1, all, mul #4
**	add	(w1[2-5]), w0, #?4
**	ld1w	{ za1v\.s\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za32_1_w0_4,
	      svld1_ver_vnum_za32 (1, w0, p0, x1, 4),
	      svld1_ver_vnum_za32 (1, w0, p0, x1, 4))

/*
** ld1_vnum_za32_3_w0_13:
**	incb	x1, all, mul #13
**	add	(w1[2-5]), w0, #?13
**	ld1w	{ za3v\.s\[\1, 0\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za32_3_w0_13,
	      svld1_ver_vnum_za32 (3, w0, p0, x1, 13),
	      svld1_ver_vnum_za32 (3, w0, p0, x1, 13))

/*
** ld1_vnum_za32_0_w0_x2:
**	cntb	(x[0-9]+)
**	madd	(x[0-9]+), (?:\1, x2|x2, \1), x1
**	add	(w1[2-5]), (?:w0, w2|w2, w0)
**	ld1w	{ za0v\.s\[\3, 0\] }, p0/z, \[\2\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za32_0_w0_x2,
	      svld1_ver_vnum_za32 (0, w0, p0, x1, x2),
	      svld1_ver_vnum_za32 (0, w0, p0, x1, x2))

/*
** ld1_vnum_za32_1_w0p1_0:
**	mov	(w1[2-5]), w0
**	ld1w	{ za1v\.s\[\1, 1\] }, p0/z, \[x1\]
**	ret
*/
TEST_LOAD_ZA (ld1_vnum_za32_1_w0p1_0,
	      svld1_ver_vnum_za32 (1, w0 + 1, p0, x1, 0),
	      svld1_ver_vnum_za32 (1, w0 + 1, p0, x1, 0))
