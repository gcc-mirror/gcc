/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** write_0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	mova	za\.d\[\1, 0, vgx2\], {z0\.d - z1\.d}
**	ret
*/
TEST_ZA_XN (write_0_z0, svfloat32x2_t,
	    svwrite_za32_f32_vg1x2 (0, z0),
	    svwrite_za32_vg1x2 (0, z0))

/*
** write_w0_z0:
**	mov	(w8|w9|w10|w11), w0
**	mova	za\.d\[\1, 0, vgx2\], {z0\.d - z1\.d}
**	ret
*/
TEST_ZA_XN (write_w0_z0, svint32x2_t,
	    svwrite_za32_s32_vg1x2 (w0, z0),
	    svwrite_za32_vg1x2 (w0, z0))

/*
** write_w7_z0:
**	mov	(w8|w9|w10|w11), w7
**	mova	za\.d\[\1, 0, vgx2\], {z0\.d - z1\.d}
**	ret
*/
TEST_ZA_XN (write_w7_z0, svuint32x2_t,
	    svwrite_za32_u32_vg1x2 (w7, z0),
	    svwrite_za32_vg1x2 (w7, z0))

/*
** write_w8_z0:
**	mova	za\.d\[w8, 0, vgx2\], {z0\.d - z1\.d}
**	ret
*/
TEST_ZA_XN (write_w8_z0, svfloat32x2_t,
	    svwrite_za32_f32_vg1x2 (w8, z0),
	    svwrite_za32_vg1x2 (w8, z0))

/*
** write_w11_z0:
**	mova	za\.d\[w11, 0, vgx2\], {z0\.d - z1\.d}
**	ret
*/
TEST_ZA_XN (write_w11_z0, svint32x2_t,
	    svwrite_za32_s32_vg1x2 (w11, z0),
	    svwrite_za32_vg1x2 (w11, z0))


/*
** write_w12_z0:
**	mov	(w8|w9|w10|w11), w12
**	mova	za\.d\[\1, 0, vgx2\], {z0\.d - z1\.d}
**	ret
*/
TEST_ZA_XN (write_w12_z0, svuint32x2_t,
	    svwrite_za32_u32_vg1x2 (w12, z0),
	    svwrite_za32_vg1x2 (w12, z0))

/*
** write_w8p7_z0:
**	mova	za\.d\[w8, 7, vgx2\], {z0\.d - z1\.d}
**	ret
*/
TEST_ZA_XN (write_w8p7_z0, svfloat32x2_t,
	    svwrite_za32_f32_vg1x2 (w8 + 7, z0),
	    svwrite_za32_vg1x2 (w8 + 7, z0))

/*
** write_w8p8_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	mova	za\.d\[\1, 0, vgx2\], {z0\.d - z1\.d}
**	ret
*/
TEST_ZA_XN (write_w8p8_z0, svint32x2_t,
	    svwrite_za32_s32_vg1x2 (w8 + 8, z0),
	    svwrite_za32_vg1x2 (w8 + 8, z0))

/*
** write_w8m1_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	mova	za\.d\[\1, 0, vgx2\], {z0\.d - z1\.d}
**	ret
*/
TEST_ZA_XN (write_w8m1_z0, svuint32x2_t,
	    svwrite_za32_u32_vg1x2 (w8 - 1, z0),
	    svwrite_za32_vg1x2 (w8 - 1, z0))

/*
** write_w8_z18:
**	mova	za\.d\[w8, 0, vgx2\], {z18\.d - z19\.d}
**	ret
*/
TEST_ZA_XN (write_w8_z18, svfloat32x2_t,
	    svwrite_za32_f32_vg1x2 (w8, z18),
	    svwrite_za32_vg1x2 (w8, z18))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** write_w8_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	mova	za\.d\[w8, 0, vgx2\], [^\n]+
**	ret
*/
TEST_ZA_XN (write_w8_z23, svint32x2_t,
	    svwrite_za32_s32_vg1x2 (w8, z23),
	    svwrite_za32_vg1x2 (w8, z23))

/*
** write_w8_z28:
**	mova	za\.d\[w8, 0, vgx2\], {z28\.d - z29\.d}
**	ret
*/
TEST_ZA_XN (write_w8_z28, svuint32x2_t,
	    svwrite_za32_u32_vg1x2 (w8, z28),
	    svwrite_za32_vg1x2 (w8, z28))
