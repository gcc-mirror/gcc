/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sme-i16i64"

#include "test_sme2_acle.h"

/*
** sub_0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	sub	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (sub_0_z0, svint64x4_t,
	    svsub_za64_s64_vg1x4 (0, z0),
	    svsub_za64_vg1x4 (0, z0))

/*
** sub_w0_z0:
**	mov	(w8|w9|w10|w11), w0
**	sub	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (sub_w0_z0, svint64x4_t,
	    svsub_za64_s64_vg1x4 (w0, z0),
	    svsub_za64_vg1x4 (w0, z0))

/*
** sub_w7_z0:
**	mov	(w8|w9|w10|w11), w7
**	sub	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (sub_w7_z0, svint64x4_t,
	    svsub_za64_s64_vg1x4 (w7, z0),
	    svsub_za64_vg1x4 (w7, z0))

/*
** sub_w8_z0:
**	sub	za\.d\[w8, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (sub_w8_z0, svint64x4_t,
	    svsub_za64_s64_vg1x4 (w8, z0),
	    svsub_za64_vg1x4 (w8, z0))

/*
** sub_w11_z0:
**	sub	za\.d\[w11, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (sub_w11_z0, svint64x4_t,
	    svsub_za64_s64_vg1x4 (w11, z0),
	    svsub_za64_vg1x4 (w11, z0))


/*
** sub_w12_z0:
**	mov	(w8|w9|w10|w11), w12
**	sub	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (sub_w12_z0, svint64x4_t,
	    svsub_za64_s64_vg1x4 (w12, z0),
	    svsub_za64_vg1x4 (w12, z0))

/*
** sub_w8p7_z0:
**	sub	za\.d\[w8, 7, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (sub_w8p7_z0, svint64x4_t,
	    svsub_za64_s64_vg1x4 (w8 + 7, z0),
	    svsub_za64_vg1x4 (w8 + 7, z0))

/*
** sub_w8p8_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	sub	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (sub_w8p8_z0, svint64x4_t,
	    svsub_za64_s64_vg1x4 (w8 + 8, z0),
	    svsub_za64_vg1x4 (w8 + 8, z0))

/*
** sub_w8m1_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	sub	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (sub_w8m1_z0, svint64x4_t,
	    svsub_za64_s64_vg1x4 (w8 - 1, z0),
	    svsub_za64_vg1x4 (w8 - 1, z0))

/*
** sub_w8_z4:
**	sub	za\.d\[w8, 0, vgx4\], {z4\.d - z7\.d}
**	ret
*/
TEST_ZA_XN (sub_w8_z4, svint64x4_t,
	    svsub_za64_s64_vg1x4 (w8, z4),
	    svsub_za64_vg1x4 (w8, z4))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** sub_w8_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sub	za\.d\[w8, 0, vgx4\], [^\n]+
**	ret
*/
TEST_ZA_XN (sub_w8_z18, svint64x4_t,
	    svsub_za64_s64_vg1x4 (w8, z18),
	    svsub_za64_vg1x4 (w8, z18))

/*
** sub_w8_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sub	za\.d\[w8, 0, vgx4\], [^\n]+
**	ret
*/
TEST_ZA_XN (sub_w8_z23, svint64x4_t,
	    svsub_za64_s64_vg1x4 (w8, z23),
	    svsub_za64_vg1x4 (w8, z23))

/*
** sub_w8_z28:
**	sub	za\.d\[w8, 0, vgx4\], {z28\.d - z31\.d}
**	ret
*/
TEST_ZA_XN (sub_w8_z28, svint64x4_t,
	    svsub_za64_s64_vg1x4 (w8, z28),
	    svsub_za64_vg1x4 (w8, z28))
