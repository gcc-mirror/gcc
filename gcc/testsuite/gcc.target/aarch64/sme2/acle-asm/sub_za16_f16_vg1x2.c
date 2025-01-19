/* { dg-do assemble { target aarch64_asm_sme-f16f16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-f16f16_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme-f16f16"

/*
** sub_0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	fsub	za\.h\[\1, 0, vgx2\], {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (sub_0_z0, svfloat16x2_t,
	    svsub_za16_f16_vg1x2 (0, z0),
	    svsub_za16_vg1x2 (0, z0))

/*
** sub_w0_z0:
**	mov	(w8|w9|w10|w11), w0
**	fsub	za\.h\[\1, 0, vgx2\], {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (sub_w0_z0, svfloat16x2_t,
	    svsub_za16_f16_vg1x2 (w0, z0),
	    svsub_za16_vg1x2 (w0, z0))

/*
** sub_w7_z0:
**	mov	(w8|w9|w10|w11), w7
**	fsub	za\.h\[\1, 0, vgx2\], {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (sub_w7_z0, svfloat16x2_t,
	    svsub_za16_f16_vg1x2 (w7, z0),
	    svsub_za16_vg1x2 (w7, z0))

/*
** sub_w8_z0:
**	fsub	za\.h\[w8, 0, vgx2\], {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (sub_w8_z0, svfloat16x2_t,
	    svsub_za16_f16_vg1x2 (w8, z0),
	    svsub_za16_vg1x2 (w8, z0))

/*
** sub_w11_z0:
**	fsub	za\.h\[w11, 0, vgx2\], {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (sub_w11_z0, svfloat16x2_t,
	    svsub_za16_f16_vg1x2 (w11, z0),
	    svsub_za16_vg1x2 (w11, z0))


/*
** sub_w12_z0:
**	mov	(w8|w9|w10|w11), w12
**	fsub	za\.h\[\1, 0, vgx2\], {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (sub_w12_z0, svfloat16x2_t,
	    svsub_za16_f16_vg1x2 (w12, z0),
	    svsub_za16_vg1x2 (w12, z0))

/*
** sub_w8p7_z0:
**	fsub	za\.h\[w8, 7, vgx2\], {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (sub_w8p7_z0, svfloat16x2_t,
	    svsub_za16_f16_vg1x2 (w8 + 7, z0),
	    svsub_za16_vg1x2 (w8 + 7, z0))

/*
** sub_w8p8_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	fsub	za\.h\[\1, 0, vgx2\], {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (sub_w8p8_z0, svfloat16x2_t,
	    svsub_za16_f16_vg1x2 (w8 + 8, z0),
	    svsub_za16_vg1x2 (w8 + 8, z0))

/*
** sub_w8m1_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	fsub	za\.h\[\1, 0, vgx2\], {z0\.h - z1\.h}
**	ret
*/
TEST_ZA_XN (sub_w8m1_z0, svfloat16x2_t,
	    svsub_za16_f16_vg1x2 (w8 - 1, z0),
	    svsub_za16_vg1x2 (w8 - 1, z0))

/*
** sub_w8_z18:
**	fsub	za\.h\[w8, 0, vgx2\], {z18\.h - z19\.h}
**	ret
*/
TEST_ZA_XN (sub_w8_z18, svfloat16x2_t,
	    svsub_za16_f16_vg1x2 (w8, z18),
	    svsub_za16_vg1x2 (w8, z18))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** sub_w8_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	fsub	za\.h\[w8, 0, vgx2\], [^\n]+
**	ret
*/
TEST_ZA_XN (sub_w8_z23, svfloat16x2_t,
	    svsub_za16_f16_vg1x2 (w8, z23),
	    svsub_za16_vg1x2 (w8, z23))

/*
** sub_w8_z28:
**	fsub	za\.h\[w8, 0, vgx2\], {z28\.h - z29\.h}
**	ret
*/
TEST_ZA_XN (sub_w8_z28, svfloat16x2_t,
	    svsub_za16_f16_vg1x2 (w8, z28),
	    svsub_za16_vg1x2 (w8, z28))
