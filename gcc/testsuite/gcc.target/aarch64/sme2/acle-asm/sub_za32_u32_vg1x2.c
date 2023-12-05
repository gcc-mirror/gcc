/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** sub_0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	sub	za\.s\[\1, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (sub_0_z0, svuint32x2_t,
	    svsub_za32_u32_vg1x2 (0, z0),
	    svsub_za32_vg1x2 (0, z0))

/*
** sub_w0_z0:
**	mov	(w8|w9|w10|w11), w0
**	sub	za\.s\[\1, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (sub_w0_z0, svuint32x2_t,
	    svsub_za32_u32_vg1x2 (w0, z0),
	    svsub_za32_vg1x2 (w0, z0))

/*
** sub_w7_z0:
**	mov	(w8|w9|w10|w11), w7
**	sub	za\.s\[\1, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (sub_w7_z0, svuint32x2_t,
	    svsub_za32_u32_vg1x2 (w7, z0),
	    svsub_za32_vg1x2 (w7, z0))

/*
** sub_w8_z0:
**	sub	za\.s\[w8, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (sub_w8_z0, svuint32x2_t,
	    svsub_za32_u32_vg1x2 (w8, z0),
	    svsub_za32_vg1x2 (w8, z0))

/*
** sub_w11_z0:
**	sub	za\.s\[w11, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (sub_w11_z0, svuint32x2_t,
	    svsub_za32_u32_vg1x2 (w11, z0),
	    svsub_za32_vg1x2 (w11, z0))


/*
** sub_w12_z0:
**	mov	(w8|w9|w10|w11), w12
**	sub	za\.s\[\1, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (sub_w12_z0, svuint32x2_t,
	    svsub_za32_u32_vg1x2 (w12, z0),
	    svsub_za32_vg1x2 (w12, z0))

/*
** sub_w8p7_z0:
**	sub	za\.s\[w8, 7, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (sub_w8p7_z0, svuint32x2_t,
	    svsub_za32_u32_vg1x2 (w8 + 7, z0),
	    svsub_za32_vg1x2 (w8 + 7, z0))

/*
** sub_w8p8_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	sub	za\.s\[\1, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (sub_w8p8_z0, svuint32x2_t,
	    svsub_za32_u32_vg1x2 (w8 + 8, z0),
	    svsub_za32_vg1x2 (w8 + 8, z0))

/*
** sub_w8m1_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	sub	za\.s\[\1, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (sub_w8m1_z0, svuint32x2_t,
	    svsub_za32_u32_vg1x2 (w8 - 1, z0),
	    svsub_za32_vg1x2 (w8 - 1, z0))

/*
** sub_w8_z18:
**	sub	za\.s\[w8, 0, vgx2\], {z18\.s - z19\.s}
**	ret
*/
TEST_ZA_XN (sub_w8_z18, svuint32x2_t,
	    svsub_za32_u32_vg1x2 (w8, z18),
	    svsub_za32_vg1x2 (w8, z18))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** sub_w8_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	sub	za\.s\[w8, 0, vgx2\], [^\n]+
**	ret
*/
TEST_ZA_XN (sub_w8_z23, svuint32x2_t,
	    svsub_za32_u32_vg1x2 (w8, z23),
	    svsub_za32_vg1x2 (w8, z23))

/*
** sub_w8_z28:
**	sub	za\.s\[w8, 0, vgx2\], {z28\.s - z29\.s}
**	ret
*/
TEST_ZA_XN (sub_w8_z28, svuint32x2_t,
	    svsub_za32_u32_vg1x2 (w8, z28),
	    svsub_za32_vg1x2 (w8, z28))
