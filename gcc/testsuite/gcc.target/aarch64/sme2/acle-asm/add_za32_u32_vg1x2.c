/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** add_0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	add	za\.s\[\1, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (add_0_z0, svuint32x2_t,
	    svadd_za32_u32_vg1x2 (0, z0),
	    svadd_za32_vg1x2 (0, z0))

/*
** add_w0_z0:
**	mov	(w8|w9|w10|w11), w0
**	add	za\.s\[\1, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (add_w0_z0, svuint32x2_t,
	    svadd_za32_u32_vg1x2 (w0, z0),
	    svadd_za32_vg1x2 (w0, z0))

/*
** add_w7_z0:
**	mov	(w8|w9|w10|w11), w7
**	add	za\.s\[\1, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (add_w7_z0, svuint32x2_t,
	    svadd_za32_u32_vg1x2 (w7, z0),
	    svadd_za32_vg1x2 (w7, z0))

/*
** add_w8_z0:
**	add	za\.s\[w8, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (add_w8_z0, svuint32x2_t,
	    svadd_za32_u32_vg1x2 (w8, z0),
	    svadd_za32_vg1x2 (w8, z0))

/*
** add_w11_z0:
**	add	za\.s\[w11, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (add_w11_z0, svuint32x2_t,
	    svadd_za32_u32_vg1x2 (w11, z0),
	    svadd_za32_vg1x2 (w11, z0))


/*
** add_w12_z0:
**	mov	(w8|w9|w10|w11), w12
**	add	za\.s\[\1, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (add_w12_z0, svuint32x2_t,
	    svadd_za32_u32_vg1x2 (w12, z0),
	    svadd_za32_vg1x2 (w12, z0))

/*
** add_w8p7_z0:
**	add	za\.s\[w8, 7, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (add_w8p7_z0, svuint32x2_t,
	    svadd_za32_u32_vg1x2 (w8 + 7, z0),
	    svadd_za32_vg1x2 (w8 + 7, z0))

/*
** add_w8p8_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	add	za\.s\[\1, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (add_w8p8_z0, svuint32x2_t,
	    svadd_za32_u32_vg1x2 (w8 + 8, z0),
	    svadd_za32_vg1x2 (w8 + 8, z0))

/*
** add_w8m1_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	add	za\.s\[\1, 0, vgx2\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (add_w8m1_z0, svuint32x2_t,
	    svadd_za32_u32_vg1x2 (w8 - 1, z0),
	    svadd_za32_vg1x2 (w8 - 1, z0))

/*
** add_w8_z18:
**	add	za\.s\[w8, 0, vgx2\], {z18\.s - z19\.s}
**	ret
*/
TEST_ZA_XN (add_w8_z18, svuint32x2_t,
	    svadd_za32_u32_vg1x2 (w8, z18),
	    svadd_za32_vg1x2 (w8, z18))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** add_w8_z23:
**	mov	[^\n]+
**	mov	[^\n]+
**	add	za\.s\[w8, 0, vgx2\], [^\n]+
**	ret
*/
TEST_ZA_XN (add_w8_z23, svuint32x2_t,
	    svadd_za32_u32_vg1x2 (w8, z23),
	    svadd_za32_vg1x2 (w8, z23))

/*
** add_w8_z28:
**	add	za\.s\[w8, 0, vgx2\], {z28\.s - z29\.s}
**	ret
*/
TEST_ZA_XN (add_w8_z28, svuint32x2_t,
	    svadd_za32_u32_vg1x2 (w8, z28),
	    svadd_za32_vg1x2 (w8, z28))
