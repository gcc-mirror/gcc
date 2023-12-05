/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define STREAMING_COMPATIBLE
#include "test_sme_acle.h"

/*
** ldr_vnum_za_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	ldr	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_LOAD_ZA (ldr_vnum_za_0_0,
	      svldr_vnum_za (0, x1, 0),
	      svldr_vnum_za (0, x1, 0))

/*
** ldr_vnum_za_0_1:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	ldr	za\[\1, 1\], \[x1(?:, #1, mul vl)?\]
**	ret
*/
TEST_LOAD_ZA (ldr_vnum_za_0_1,
	      svldr_vnum_za (0, x1, 1),
	      svldr_vnum_za (0, x1, 1))

/*
** ldr_vnum_za_1_0:
**	mov	(w1[2-5]), #?1
**	ldr	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_LOAD_ZA (ldr_vnum_za_1_0,
	      svldr_vnum_za (1, x1, 0),
	      svldr_vnum_za (1, x1, 0))

/*
** ldr_vnum_za_1_2:
**	mov	(w1[2-5]), #?1
**	ldr	za\[\1, 2\], \[x1(?:, #2, mul vl)?\]
**	ret
*/
TEST_LOAD_ZA (ldr_vnum_za_1_2,
	      svldr_vnum_za (1, x1, 2),
	      svldr_vnum_za (1, x1, 2))

/*
** ldr_vnum_za_w0_0:
**	mov	(w1[2-5]), w0
**	ldr	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_LOAD_ZA (ldr_vnum_za_w0_0,
	      svldr_vnum_za (w0, x1, 0),
	      svldr_vnum_za (w0, x1, 0))

/*
** ldr_vnum_za_w0_1:
**	mov	(w1[2-5]), w0
**	ldr	za\[\1, 1\], \[x1, #1, mul vl\]
**	ret
*/
TEST_LOAD_ZA (ldr_vnum_za_w0_1,
	      svldr_vnum_za (w0, x1, 1),
	      svldr_vnum_za (w0, x1, 1))

/*
** ldr_vnum_za_w0_13:
**	mov	(w1[2-5]), w0
**	ldr	za\[\1, 13\], \[x1, #13, mul vl\]
**	ret
*/
TEST_LOAD_ZA (ldr_vnum_za_w0_13,
	      svldr_vnum_za (w0, x1, 13),
	      svldr_vnum_za (w0, x1, 13))

/*
** ldr_vnum_za_w0_15:
**	mov	(w1[2-5]), w0
**	ldr	za\[\1, 15\], \[x1, #15, mul vl\]
**	ret
*/
TEST_LOAD_ZA (ldr_vnum_za_w0_15,
	      svldr_vnum_za (w0, x1, 15),
	      svldr_vnum_za (w0, x1, 15))

/*
** ldr_vnum_za_w0_16:
** (
**	add	(w1[2-5]), w0, #?16
**	addsvl	(x[0-9]+), x1, #16
**	ldr	za\[\1, 0\], \[\2(?:, #0, mul vl)?\]
** |
**	addsvl	(x[0-9]+), x1, #16
**	add	(w1[2-5]), w0, #?16
**	ldr	za\[\4, 0\], \[\3(?:, #0, mul vl)?\]
** )
**	ret
*/
TEST_LOAD_ZA (ldr_vnum_za_w0_16,
	      svldr_vnum_za (w0, x1, 16),
	      svldr_vnum_za (w0, x1, 16))

/*
** ldr_vnum_za_w0_m1:
** (
**	sub	(w1[2-5]), w0, #?1
**	addsvl	(x[0-9]+), x1, #-1
**	ldr	za\[\1, 0\], \[\2(?:, #0, mul vl)?\]
** |
**	addsvl	(x[0-9]+), x1, #-1
**	sub	(w1[2-5]), w0, #?1
**	ldr	za\[\4, 0\], \[\3(?:, #0, mul vl)?\]
** )
**	ret
*/
TEST_LOAD_ZA (ldr_vnum_za_w0_m1,
	      svldr_vnum_za (w0, x1, -1),
	      svldr_vnum_za (w0, x1, -1))

/*
** ldr_vnum_za_w0p1_0:
**	add	(w1[2-5]), w0, #?1
**	ldr	za\[\1, 0\], \[x1(?:, #0, mul vl)?\]
**	ret
*/
TEST_LOAD_ZA (ldr_vnum_za_w0p1_0,
	      svldr_vnum_za (w0 + 1, x1, 0),
	      svldr_vnum_za (w0 + 1, x1, 0))

/*
** ldr_vnum_za_w0m1_1:
**	sub	(w1[2-5]), w0, #?1
**	ldr	za\[\1, 1\], \[x1(?:, #1, mul vl)?\]
**	ret
*/
TEST_LOAD_ZA (ldr_vnum_za_w0m1_1,
	      svldr_vnum_za (w0 - 1, x1, 1),
	      svldr_vnum_za (w0 - 1, x1, 1))

/*
** ldr_vnum_za_w0p2_3:
**	add	(w1[2-5]), w0, #?2
**	ldr	za\[\1, 3\], \[x1(?:, #3, mul vl)?\]
**	ret
*/
TEST_LOAD_ZA (ldr_vnum_za_w0p2_3,
	      svldr_vnum_za (w0 + 2, x1, 3),
	      svldr_vnum_za (w0 + 2, x1, 3))
