/* { dg-do assemble { target aarch64_asm_sme2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme2p1"

/*
** zero_0:
**	mov	(w8|w9|w10|w11), #?0
**	zero	za\.d\[\1, 0:1\]
**	ret
*/
TEST_ZA_XN (zero_0, svint8_t,
	    svzero_za64_vg2x1 (0),
	    svzero_za64_vg2x1 (0))

/*
** zero_w0:
**	mov	(w8|w9|w10|w11), w0
**	zero	za\.d\[\1, 0:1\]
**	ret
*/
TEST_ZA_XN (zero_w0, svint8_t,
	    svzero_za64_vg2x1 (w0),
	    svzero_za64_vg2x1 (w0))

/*
** zero_w0p1:
**	add	(w8|w9|w10|w11), w0, #?1
**	zero	za\.d\[\1, 0:1\]
**	ret
*/
TEST_ZA_XN (zero_w0p1, svint8_t,
	    svzero_za64_vg2x1 (w0 + 1),
	    svzero_za64_vg2x1 (w0 + 1))

/*
** zero_w0p2:
**	mov	(w8|w9|w10|w11), w0
**	zero	za\.d\[\1, 2:3\]
**	ret
*/
TEST_ZA_XN (zero_w0p2, svint8_t,
	    svzero_za64_vg2x1 (w0 + 2),
	    svzero_za64_vg2x1 (w0 + 2))

/*
** zero_w0p3:
**	add	(w8|w9|w10|w11), w0, #?3
**	zero	za\.d\[\1, 0:1\]
**	ret
*/
TEST_ZA_XN (zero_w0p3, svint8_t,
	    svzero_za64_vg2x1 (w0 + 3),
	    svzero_za64_vg2x1 (w0 + 3))

/*
** zero_w0p4:
**	mov	(w8|w9|w10|w11), w0
**	zero	za\.d\[\1, 4:5\]
**	ret
*/
TEST_ZA_XN (zero_w0p4, svint8_t,
	    svzero_za64_vg2x1 (w0 + 4),
	    svzero_za64_vg2x1 (w0 + 4))

/*
** zero_w0p6:
**	mov	(w8|w9|w10|w11), w0
**	zero	za\.d\[\1, 6:7\]
**	ret
*/
TEST_ZA_XN (zero_w0p6, svint8_t,
	    svzero_za64_vg2x1 (w0 + 6),
	    svzero_za64_vg2x1 (w0 + 6))

/*
** zero_w0p8:
**	mov	(w8|w9|w10|w11), w0
**	zero	za\.d\[\1, 8:9\]
**	ret
*/
TEST_ZA_XN (zero_w0p8, svint8_t,
	    svzero_za64_vg2x1 (w0 + 8),
	    svzero_za64_vg2x1 (w0 + 8))

/*
** zero_w0p14:
**	mov	(w8|w9|w10|w11), w0
**	zero	za\.d\[\1, 14:15\]
**	ret
*/
TEST_ZA_XN (zero_w0p14, svint8_t,
	    svzero_za64_vg2x1 (w0 + 14),
	    svzero_za64_vg2x1 (w0 + 14))

/*
** zero_w0p16:
**	add	(w8|w9|w10|w11), w0, #?16
**	zero	za\.d\[\1, 0:1\]
**	ret
*/
TEST_ZA_XN (zero_w0p16, svint8_t,
	    svzero_za64_vg2x1 (w0 + 16),
	    svzero_za64_vg2x1 (w0 + 16))

/*
** zero_w0m1:
**	sub	(w8|w9|w10|w11), w0, #?1
**	zero	za\.d\[\1, 0:1\]
**	ret
*/
TEST_ZA_XN (zero_w0m1, svint8_t,
	    svzero_za64_vg2x1 (w0 - 1),
	    svzero_za64_vg2x1 (w0 - 1))
