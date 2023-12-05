/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** write_za32_s32_z0_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	za0v\.s\[\1, 0:3\], {z0\.s - z3\.s}
**	ret
*/
TEST_ZA_XN (write_za32_s32_z0_0_0, svint32x4_t,
	    svwrite_ver_za32_s32_vg4 (0, 0, z0),
	    svwrite_ver_za32_s32_vg4 (0, 0, z0))

/*
** write_za32_u32_z4_1_1:
**	mov	(w1[2-5]), #?1
**	mova	za1v\.s\[\1, 0:3\], {z4\.s - z7\.s}
**	ret
*/
TEST_ZA_XN (write_za32_u32_z4_1_1, svuint32x4_t,
	    svwrite_ver_za32_u32_vg4 (1, 1, z4),
	    svwrite_ver_za32_u32_vg4 (1, 1, z4))

/*
** write_za32_f32_z28_2_w11:
**	mov	(w1[2-5]), w11
**	mova	za2v\.s\[\1, 0:3\], {z28\.s - z31\.s}
**	ret
*/
TEST_ZA_XN (write_za32_f32_z28_2_w11, svfloat32x4_t,
	    svwrite_ver_za32_f32_vg4 (2, w11, z28),
	    svwrite_ver_za32_f32_vg4 (2, w11, z28))

/*
** write_za32_s32_z0_3_w12:
**	mova	za3v\.s\[w12, 0:3\], {z0\.s - z3\.s}
**	ret
*/
TEST_ZA_XN (write_za32_s32_z0_3_w12, svint32x4_t,
	    svwrite_ver_za32_s32_vg4 (3, w12, z0),
	    svwrite_ver_za32_s32_vg4 (3, w12, z0))

/*
** write_za32_u32_z18_0_w15:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mova	za0v\.s\[w15, 0:3\], {[^\n]+}
**	ret
*/
TEST_ZA_XN (write_za32_u32_z18_0_w15, svuint32x4_t,
	    svwrite_ver_za32_u32_vg4 (0, w15, z18),
	    svwrite_ver_za32_u32_vg4 (0, w15, z18))

/*
** write_za32_f32_z23_1_w12p4:
**	add	(w[0-9]+), w12, #?4
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mova	za1v\.s\[\1, 0:3\], {[^\n]+}
**	ret
*/
TEST_ZA_XN (write_za32_f32_z23_1_w12p4, svfloat32x4_t,
	    svwrite_ver_za32_f32_vg4 (1, w12 + 4, z23),
	    svwrite_ver_za32_f32_vg4 (1, w12 + 4, z23))

/*
** write_za32_u32_z4_2_w12p1:
**	add	(w[0-9]+), w12, #?1
**	mova	za2v\.s\[\1, 0:3\], {z4\.s - z7\.s}
**	ret
*/
TEST_ZA_XN (write_za32_u32_z4_2_w12p1, svuint32x4_t,
	    svwrite_ver_za32_u32_vg4 (2, w12 + 1, z4),
	    svwrite_ver_za32_u32_vg4 (2, w12 + 1, z4))

/*
** write_za32_s32_z28_3_w12p2:
**	add	(w[0-9]+), w12, #?2
**	mova	za3v\.s\[\1, 0:3\], {z28\.s - z31\.s}
**	ret
*/
TEST_ZA_XN (write_za32_s32_z28_3_w12p2, svint32x4_t,
	    svwrite_ver_za32_s32_vg4 (3, w12 + 2, z28),
	    svwrite_ver_za32_s32_vg4 (3, w12 + 2, z28))

/*
** write_za32_f32_z0_0_w15p3:
**	add	(w[0-9]+), w15, #?3
**	mova	za0v\.s\[\1, 0:3\], {z0\.s - z3\.s}
**	ret
*/
TEST_ZA_XN (write_za32_f32_z0_0_w15p3, svfloat32x4_t,
	    svwrite_ver_za32_f32_vg4 (0, w15 + 3, z0),
	    svwrite_ver_za32_f32_vg4 (0, w15 + 3, z0))

/*
** write_za32_u32_z28_1_w12p4:
**	add	(w[0-9]+), w12, #?4
**	mova	za1v\.s\[\1, 0:3\], {z28\.s - z31\.s}
**	ret
*/
TEST_ZA_XN (write_za32_u32_z28_1_w12p4, svuint32x4_t,
	    svwrite_ver_za32_u32_vg4 (1, w12 + 4, z28),
	    svwrite_ver_za32_u32_vg4 (1, w12 + 4, z28))

/*
** write_za32_f32_z4_2_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	mova	za2v\.s\[\1, 0:3\], {z4\.s - z7\.s}
**	ret
*/
TEST_ZA_XN (write_za32_f32_z4_2_w12m1, svfloat32x4_t,
	    svwrite_ver_za32_f32_vg4 (2, w12 - 1, z4),
	    svwrite_ver_za32_f32_vg4 (2, w12 - 1, z4))

/*
** write_za32_u32_z28_3_w16:
**	mov	(w1[2-5]), w16
**	mova	za3v\.s\[\1, 0:3\], {z28\.s - z31\.s}
**	ret
*/
TEST_ZA_XN (write_za32_u32_z28_3_w16, svuint32x4_t,
	    svwrite_ver_za32_u32_vg4 (3, w16, z28),
	    svwrite_ver_za32_u32_vg4 (3, w16, z28))
