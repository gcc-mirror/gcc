/* { dg-do assemble { target aarch64_asm_sme-b16b16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme-b16b16_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

#pragma GCC target "+sme-b16b16"

/*
** sub_0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	bfsub	za\.h\[\1, 0, vgx4\], {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (sub_0_z0, svbfloat16x4_t,
	    svsub_za16_bf16_vg1x4 (0, z0),
	    svsub_za16_vg1x4 (0, z0))

/*
** sub_w0_z0:
**	mov	(w8|w9|w10|w11), w0
**	bfsub	za\.h\[\1, 0, vgx4\], {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (sub_w0_z0, svbfloat16x4_t,
	    svsub_za16_bf16_vg1x4 (w0, z0),
	    svsub_za16_vg1x4 (w0, z0))

/*
** sub_w7_z0:
**	mov	(w8|w9|w10|w11), w7
**	bfsub	za\.h\[\1, 0, vgx4\], {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (sub_w7_z0, svbfloat16x4_t,
	    svsub_za16_bf16_vg1x4 (w7, z0),
	    svsub_za16_vg1x4 (w7, z0))

/*
** sub_w8_z0:
**	bfsub	za\.h\[w8, 0, vgx4\], {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (sub_w8_z0, svbfloat16x4_t,
	    svsub_za16_bf16_vg1x4 (w8, z0),
	    svsub_za16_vg1x4 (w8, z0))

/*
** sub_w11_z0:
**	bfsub	za\.h\[w11, 0, vgx4\], {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (sub_w11_z0, svbfloat16x4_t,
	    svsub_za16_bf16_vg1x4 (w11, z0),
	    svsub_za16_vg1x4 (w11, z0))


/*
** sub_w12_z0:
**	mov	(w8|w9|w10|w11), w12
**	bfsub	za\.h\[\1, 0, vgx4\], {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (sub_w12_z0, svbfloat16x4_t,
	    svsub_za16_bf16_vg1x4 (w12, z0),
	    svsub_za16_vg1x4 (w12, z0))

/*
** sub_w8p7_z0:
**	bfsub	za\.h\[w8, 7, vgx4\], {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (sub_w8p7_z0, svbfloat16x4_t,
	    svsub_za16_bf16_vg1x4 (w8 + 7, z0),
	    svsub_za16_vg1x4 (w8 + 7, z0))

/*
** sub_w8p8_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	bfsub	za\.h\[\1, 0, vgx4\], {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (sub_w8p8_z0, svbfloat16x4_t,
	    svsub_za16_bf16_vg1x4 (w8 + 8, z0),
	    svsub_za16_vg1x4 (w8 + 8, z0))

/*
** sub_w8m1_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	bfsub	za\.h\[\1, 0, vgx4\], {z0\.h - z3\.h}
**	ret
*/
TEST_ZA_XN (sub_w8m1_z0, svbfloat16x4_t,
	    svsub_za16_bf16_vg1x4 (w8 - 1, z0),
	    svsub_za16_vg1x4 (w8 - 1, z0))

/*
** sub_w8_z4:
**	bfsub	za\.h\[w8, 0, vgx4\], {z4\.h - z7\.h}
**	ret
*/
TEST_ZA_XN (sub_w8_z4, svbfloat16x4_t,
	    svsub_za16_bf16_vg1x4 (w8, z4),
	    svsub_za16_vg1x4 (w8, z4))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** sub_w8_z18:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	bfsub	za\.h\[w8, 0, vgx4\], [^\n]+
**	ret
*/
TEST_ZA_XN (sub_w8_z18, svbfloat16x4_t,
	    svsub_za16_bf16_vg1x4 (w8, z18),
	    svsub_za16_vg1x4 (w8, z18))

/*
** sub_w8_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	bfsub	za\.h\[w8, 0, vgx4\], [^\n]+
**	ret
*/
TEST_ZA_XN (sub_w8_z23, svbfloat16x4_t,
	    svsub_za16_bf16_vg1x4 (w8, z23),
	    svsub_za16_vg1x4 (w8, z23))

/*
** sub_w8_z28:
**	bfsub	za\.h\[w8, 0, vgx4\], {z28\.h - z31\.h}
**	ret
*/
TEST_ZA_XN (sub_w8_z28, svbfloat16x4_t,
	    svsub_za16_bf16_vg1x4 (w8, z28),
	    svsub_za16_vg1x4 (w8, z28))
