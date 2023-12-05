/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#pragma GCC target "+sme-i16i64"

#include "test_sme2_acle.h"

/*
** add_0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	add	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (add_0_z0, svuint64x4_t,
	    svadd_za64_u64_vg1x4 (0, z0),
	    svadd_za64_vg1x4 (0, z0))

/*
** add_w0_z0:
**	mov	(w8|w9|w10|w11), w0
**	add	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (add_w0_z0, svuint64x4_t,
	    svadd_za64_u64_vg1x4 (w0, z0),
	    svadd_za64_vg1x4 (w0, z0))

/*
** add_w7_z0:
**	mov	(w8|w9|w10|w11), w7
**	add	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (add_w7_z0, svuint64x4_t,
	    svadd_za64_u64_vg1x4 (w7, z0),
	    svadd_za64_vg1x4 (w7, z0))

/*
** add_w8_z0:
**	add	za\.d\[w8, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (add_w8_z0, svuint64x4_t,
	    svadd_za64_u64_vg1x4 (w8, z0),
	    svadd_za64_vg1x4 (w8, z0))

/*
** add_w11_z0:
**	add	za\.d\[w11, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (add_w11_z0, svuint64x4_t,
	    svadd_za64_u64_vg1x4 (w11, z0),
	    svadd_za64_vg1x4 (w11, z0))


/*
** add_w12_z0:
**	mov	(w8|w9|w10|w11), w12
**	add	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (add_w12_z0, svuint64x4_t,
	    svadd_za64_u64_vg1x4 (w12, z0),
	    svadd_za64_vg1x4 (w12, z0))

/*
** add_w8p7_z0:
**	add	za\.d\[w8, 7, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (add_w8p7_z0, svuint64x4_t,
	    svadd_za64_u64_vg1x4 (w8 + 7, z0),
	    svadd_za64_vg1x4 (w8 + 7, z0))

/*
** add_w8p8_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	add	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (add_w8p8_z0, svuint64x4_t,
	    svadd_za64_u64_vg1x4 (w8 + 8, z0),
	    svadd_za64_vg1x4 (w8 + 8, z0))

/*
** add_w8m1_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	add	za\.d\[\1, 0, vgx4\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (add_w8m1_z0, svuint64x4_t,
	    svadd_za64_u64_vg1x4 (w8 - 1, z0),
	    svadd_za64_vg1x4 (w8 - 1, z0))

/*
** add_w8_z4:
**	add	za\.d\[w8, 0, vgx4\], {z4\.d - z7\.d}
**	ret
*/
TEST_ZA_XN (add_w8_z4, svuint64x4_t,
	    svadd_za64_u64_vg1x4 (w8, z4),
	    svadd_za64_vg1x4 (w8, z4))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** add_w8_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	add	za\.d\[w8, 0, vgx4\], [^\n]+
**	ret
*/
TEST_ZA_XN (add_w8_z18, svuint64x4_t,
	    svadd_za64_u64_vg1x4 (w8, z18),
	    svadd_za64_vg1x4 (w8, z18))

/*
** add_w8_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	add	za\.d\[w8, 0, vgx4\], [^\n]+
**	ret
*/
TEST_ZA_XN (add_w8_z23, svuint64x4_t,
	    svadd_za64_u64_vg1x4 (w8, z23),
	    svadd_za64_vg1x4 (w8, z23))

/*
** add_w8_z28:
**	add	za\.d\[w8, 0, vgx4\], {z28\.d - z31\.d}
**	ret
*/
TEST_ZA_XN (add_w8_z28, svuint64x4_t,
	    svadd_za64_u64_vg1x4 (w8, z28),
	    svadd_za64_vg1x4 (w8, z28))
