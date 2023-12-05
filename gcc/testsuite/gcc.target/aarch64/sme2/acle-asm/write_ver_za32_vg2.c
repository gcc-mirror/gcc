/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** write_za32_s32_z0_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	za0v\.s\[\1, 0:1\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (write_za32_s32_z0_0_0, svint32x2_t,
	    svwrite_ver_za32_s32_vg2 (0, 0, z0),
	    svwrite_ver_za32_s32_vg2 (0, 0, z0))

/*
** write_za32_u32_z4_1_1:
**	mov	(w1[2-5]), #?1
**	mova	za1v\.s\[\1, 0:1\], {z4\.s - z5\.s}
**	ret
*/
TEST_ZA_XN (write_za32_u32_z4_1_1, svuint32x2_t,
	    svwrite_ver_za32_u32_vg2 (1, 1, z4),
	    svwrite_ver_za32_u32_vg2 (1, 1, z4))

/*
** write_za32_f32_z28_2_w11:
**	mov	(w1[2-5]), w11
**	mova	za2v\.s\[\1, 0:1\], {z28\.s - z29\.s}
**	ret
*/
TEST_ZA_XN (write_za32_f32_z28_2_w11, svfloat32x2_t,
	    svwrite_ver_za32_f32_vg2 (2, w11, z28),
	    svwrite_ver_za32_f32_vg2 (2, w11, z28))

/*
** write_za32_f32_z0_3_w12:
**	mova	za3v\.s\[w12, 0:1\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (write_za32_f32_z0_3_w12, svfloat32x2_t,
	    svwrite_ver_za32_f32_vg2 (3, w12, z0),
	    svwrite_ver_za32_f32_vg2 (3, w12, z0))

/*
** write_za32_u32_z18_0_w15:
**	mova	za0v\.s\[w15, 0:1\], {z18\.s - z19\.s}
**	ret
*/
TEST_ZA_XN (write_za32_u32_z18_0_w15, svuint32x2_t,
	    svwrite_ver_za32_u32_vg2 (0, w15, z18),
	    svwrite_ver_za32_u32_vg2 (0, w15, z18))

/*
** write_za32_s32_z23_1_w12p2:
**	mov	[^\n]+
**	mov	[^\n]+
**	mova	za1v\.s\[w12, 2:3\], {[^\n]+}
**	ret
*/
TEST_ZA_XN (write_za32_s32_z23_1_w12p2, svint32x2_t,
	    svwrite_ver_za32_s32_vg2 (1, w12 + 2, z23),
	    svwrite_ver_za32_s32_vg2 (1, w12 + 2, z23))

/*
** write_za32_f32_z4_2_w12p1:
**	add	(w[0-9]+), w12, #?1
**	mova	za2v\.s\[\1, 0:1\], {z4\.s - z5\.s}
**	ret
*/
TEST_ZA_XN (write_za32_f32_z4_2_w12p1, svfloat32x2_t,
	    svwrite_ver_za32_f32_vg2 (2, w12 + 1, z4),
	    svwrite_ver_za32_f32_vg2 (2, w12 + 1, z4))

/*
** write_za32_u32_z0_3_w15p3:
**	add	(w[0-9]+), w15, #?3
**	mova	za3v\.s\[\1, 0:1\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (write_za32_u32_z0_3_w15p3, svuint32x2_t,
	    svwrite_ver_za32_u32_vg2 (3, w15 + 3, z0),
	    svwrite_ver_za32_u32_vg2 (3, w15 + 3, z0))

/*
** write_za32_s32_z0_1_w15p4:
**	add	(w[0-9]+), w15, #?4
**	mova	za1v\.s\[\1, 0:1\], {z0\.s - z1\.s}
**	ret
*/
TEST_ZA_XN (write_za32_s32_z0_1_w15p4, svint32x2_t,
	    svwrite_ver_za32_s32_vg2 (1, w15 + 4, z0),
	    svwrite_ver_za32_s32_vg2 (1, w15 + 4, z0))

/*
** write_za32_u32_z4_3_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	mova	za3v\.s\[\1, 0:1\], {z4\.s - z5\.s}
**	ret
*/
TEST_ZA_XN (write_za32_u32_z4_3_w12m1, svuint32x2_t,
	    svwrite_ver_za32_u32_vg2 (3, w12 - 1, z4),
	    svwrite_ver_za32_u32_vg2 (3, w12 - 1, z4))

/*
** write_za32_u32_z18_1_w16:
**	mov	(w1[2-5]), w16
**	mova	za1v\.s\[\1, 0:1\], {z18\.s - z19\.s}
**	ret
*/
TEST_ZA_XN (write_za32_u32_z18_1_w16, svuint32x2_t,
	    svwrite_ver_za32_u32_vg2 (1, w16, z18),
	    svwrite_ver_za32_u32_vg2 (1, w16, z18))
