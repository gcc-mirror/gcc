/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define STREAMING_COMPATIBLE
#include "test_sme_acle.h"

/*
** str_vnum_za_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	str	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_STORE_ZA (str_vnum_za_0_0,
	       svstr_vnum_za (0, x1, 0),
	       svstr_vnum_za (0, x1, 0))

/*
** str_vnum_za_0_1:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	str	za\[\1, 1\], \[x1(?:, #1, mul vl)?\]
**	ret
*/
TEST_STORE_ZA (str_vnum_za_0_1,
	       svstr_vnum_za (0, x1, 1),
	       svstr_vnum_za (0, x1, 1))

/*
** str_vnum_za_1_0:
**	mov	(w1[2-5]), #?1
**	str	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_STORE_ZA (str_vnum_za_1_0,
	       svstr_vnum_za (1, x1, 0),
	       svstr_vnum_za (1, x1, 0))

/*
** str_vnum_za_1_2:
**	mov	(w1[2-5]), #?1
**	str	za\[\1, 2\], \[x1(?:, #2, mul vl)?\]
**	ret
*/
TEST_STORE_ZA (str_vnum_za_1_2,
	       svstr_vnum_za (1, x1, 2),
	       svstr_vnum_za (1, x1, 2))

/*
** str_vnum_za_w0_0:
**	mov	(w1[2-5]), w0
**	str	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_STORE_ZA (str_vnum_za_w0_0,
	       svstr_vnum_za (w0, x1, 0),
	       svstr_vnum_za (w0, x1, 0))

/*
** str_vnum_za_w0_1:
**	mov	(w1[2-5]), w0
**	str	za\[\1, 1\], \[x1, #1, mul vl\]
**	ret
*/
TEST_STORE_ZA (str_vnum_za_w0_1,
	       svstr_vnum_za (w0, x1, 1),
	       svstr_vnum_za (w0, x1, 1))

/*
** str_vnum_za_w0_13:
**	mov	(w1[2-5]), w0
**	str	za\[\1, 13\], \[x1, #13, mul vl\]
**	ret
*/
TEST_STORE_ZA (str_vnum_za_w0_13,
	       svstr_vnum_za (w0, x1, 13),
	       svstr_vnum_za (w0, x1, 13))

/*
** str_vnum_za_w0_15:
**	mov	(w1[2-5]), w0
**	str	za\[\1, 15\], \[x1, #15, mul vl\]
**	ret
*/
TEST_STORE_ZA (str_vnum_za_w0_15,
	       svstr_vnum_za (w0, x1, 15),
	       svstr_vnum_za (w0, x1, 15))

/*
** str_vnum_za_w0_16:
** (
**	add	(w1[2-5]), w0, #?16
**	addsvl	(x[0-9]+), x1, #16
**	str	za\[\1, 0\], \[\2(?:, #0, mul vl)?\]
** |
**	addsvl	(x[0-9]+), x1, #16
**	add	(w1[2-5]), w0, #?16
**	str	za\[\4, 0\], \[\3(?:, #0, mul vl)?\]
** )
**	ret
*/
TEST_STORE_ZA (str_vnum_za_w0_16,
	       svstr_vnum_za (w0, x1, 16),
	       svstr_vnum_za (w0, x1, 16))

/*
** str_vnum_za_w0_m1:
** (
**	sub	(w1[2-5]), w0, #?1
**	addsvl	(x[0-9]+), x1, #-1
**	str	za\[\1, 0\], \[\2(?:, #0, mul vl)?\]
** |
**	addsvl	(x[0-9]+), x1, #-1
**	sub	(w1[2-5]), w0, #?1
**	str	za\[\4, 0\], \[\3(?:, #0, mul vl)?\]
** )
**	ret
*/
TEST_STORE_ZA (str_vnum_za_w0_m1,
	       svstr_vnum_za (w0, x1, -1),
	       svstr_vnum_za (w0, x1, -1))

/*
** str_vnum_za_w0p1_0:
**	add	(w1[2-5]), w0, #?1
**	str	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_STORE_ZA (str_vnum_za_w0p1_0,
	       svstr_vnum_za (w0 + 1, x1, 0),
	       svstr_vnum_za (w0 + 1, x1, 0))

/*
** str_vnum_za_w0m1_1:
**	sub	(w1[2-5]), w0, #?1
**	str	za\[\1, 1\], \[x1(?:, #1, mul vl)?\]
**	ret
*/
TEST_STORE_ZA (str_vnum_za_w0m1_1,
	       svstr_vnum_za (w0 - 1, x1, 1),
	       svstr_vnum_za (w0 - 1, x1, 1))

/*
** str_vnum_za_w0p2_3:
**	add	(w1[2-5]), w0, #?2
**	str	za\[\1, 3\], \[x1(?:, #3, mul vl)?\]
**	ret
*/
TEST_STORE_ZA (str_vnum_za_w0p2_3,
	       svstr_vnum_za (w0 + 2, x1, 3),
	       svstr_vnum_za (w0 + 2, x1, 3))
