/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme_acle.h"

/*
** str_za_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	str	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_STORE_ZA (str_za_0,
	       svstr_za (0, x1),
	       svstr_za (0, x1))

/*
** str_za_1:
**	mov	(w1[2-5]), #?1
**	str	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_STORE_ZA (str_za_1,
	       svstr_za (1, x1),
	       svstr_za (1, x1))

/*
** str_za_w0:
**	mov	(w1[2-5]), w0
**	str	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_STORE_ZA (str_za_w0,
	       svstr_za (w0, x1),
	       svstr_za (w0, x1))

/*
** str_za_w0_1_vnum:
**	mov	(w1[2-5]), w0
**	str	za\[\1, 1\], \[x1, #1, mul vl\]
**	ret
*/
TEST_STORE_ZA (str_za_w0_1_vnum,
	       svstr_za (w0 + 1, x1 + svcntsb ()),
	       svstr_za (w0 + 1, x1 + svcntsb ()))

/*
** str_za_w0_13_vnum:
**	mov	(w1[2-5]), w0
**	str	za\[\1, 13\], \[x1, #13, mul vl\]
**	ret
*/
TEST_STORE_ZA (str_za_w0_13_vnum,
	       svstr_za (w0 + 13, x1 + svcntsb () * 13),
	       svstr_za (w0 + 13, x1 + svcntsb () * 13))

/*
** str_za_w0_15_vnum:
**	mov	(w1[2-5]), w0
**	str	za\[\1, 15\], \[x1, #15, mul vl\]
**	ret
*/
TEST_STORE_ZA (str_za_w0_15_vnum,
	       svstr_za (w0 + 15, x1 + svcntsb () * 15),
	       svstr_za (w0 + 15, x1 + svcntsb () * 15))

/*
** str_za_w0_16_vnum:
** (
**	add	(w1[2-5]), w0, #?16
**	incb	x1, all, mul #16
**	str	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
** |
**	incb	x1, all, mul #16
**	add	(w1[2-5]), w0, #?16
**	str	za\[\2, 0\], \[x1(?:, #0, mul vl)?\]
** )
**	ret
*/
TEST_STORE_ZA (str_za_w0_16_vnum,
	       svstr_za (w0 + 16, x1 + svcntsb () * 16),
	       svstr_za (w0 + 16, x1 + svcntsb () * 16))

/*
** str_za_w0_m1_vnum:
** (
**	sub	(w1[2-5]), w0, #?1
**	decb	x1
**	str	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
** |
**	decb	x1
**	sub	(w1[2-5]), w0, #?1
**	str	za\[\2, 0\], \[x1(?:, #0, mul vl)?\]
** )
**	ret
*/
TEST_STORE_ZA (str_za_w0_m1_vnum,
	       svstr_za (w0 - 1, x1 - svcntsb ()),
	       svstr_za (w0 - 1, x1 - svcntsb ()))

/*
** str_za_w0p2:
**	add	(w1[2-5]), w0, #?2
**	str	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_STORE_ZA (str_za_w0p2,
	       svstr_za (w0 + 2, x1),
	       svstr_za (w0 + 2, x1))

/*
** str_za_offset:
** (
**	mov	(w1[2-5]), w0
**	add	(x[0-9]+), x1, #?1
**	str	za\[\1, 0\], \[\2(?:, #0, mul vl)?\]
** |
**	add	(x[0-9]+), x1, #?1
**	mov	(w1[2-5]), w0
**	str	za\[\4, 0\], \[\3(?:, #0, mul vl)?\]
** )
**	ret
*/
TEST_STORE_ZA (str_za_offset,
	       svstr_za (w0, x1 + 1),
	       svstr_za (w0, x1 + 1))
