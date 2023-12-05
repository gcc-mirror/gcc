/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define STREAMING_COMPATIBLE
#include "test_sme_acle.h"

/*
** ldr_za_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	ldr	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_LOAD_ZA (ldr_za_0,
	      svldr_za (0, x1),
	      svldr_za (0, x1))

/*
** ldr_za_1:
**	mov	(w1[2-5]), #?1
**	ldr	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_LOAD_ZA (ldr_za_1,
	      svldr_za (1, x1),
	      svldr_za (1, x1))

/*
** ldr_za_w0:
**	mov	(w1[2-5]), w0
**	ldr	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_LOAD_ZA (ldr_za_w0,
	      svldr_za (w0, x1),
	      svldr_za (w0, x1))

/*
** ldr_za_w0_1_vnum:
**	mov	(w1[2-5]), w0
**	ldr	za\[\1, 1\], \[x1, #1, mul vl\]
**	ret
*/
TEST_LOAD_ZA (ldr_za_w0_1_vnum,
	      svldr_za (w0 + 1, x1 + svcntsb ()),
	      svldr_za (w0 + 1, x1 + svcntsb ()))

/*
** ldr_za_w0p2:
**	add	(w1[2-5]), w0, #?2
**	ldr	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_LOAD_ZA (ldr_za_w0p2,
	      svldr_za (w0 + 2, x1),
	      svldr_za (w0 + 2, x1))

/*
** ldr_za_offset:
** (
**	mov	(w1[2-5]), w0
**	add	(x[0-9]+), x1, #?1
**	ldr	za\[\1, 0\], \[\2(?:, #0, mul vl)?\]
** |
**	add	(x[0-9]+), x1, #?1
**	mov	(w1[2-5]), w0
**	ldr	za\[\4, 0\], \[\3(?:, #0, mul vl)?\]
** )
**	ret
*/
TEST_LOAD_ZA (ldr_za_offset,
	      svldr_za (w0, x1 + 1),
	      svldr_za (w0, x1 + 1))
