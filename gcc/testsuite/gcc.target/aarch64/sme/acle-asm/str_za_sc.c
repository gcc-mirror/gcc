/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define STREAMING_COMPATIBLE
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
