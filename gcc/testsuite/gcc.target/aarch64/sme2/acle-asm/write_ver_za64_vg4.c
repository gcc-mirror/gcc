/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** write_za64_s64_z0_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	za0v\.d\[\1, 0:3\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (write_za64_s64_z0_0_0, svint64x4_t,
	    svwrite_ver_za64_s64_vg4 (0, 0, z0),
	    svwrite_ver_za64_s64_vg4 (0, 0, z0))

/*
** write_za64_u64_z4_1_1:
**	mov	(w1[2-5]), #?1
**	mova	za1v\.d\[\1, 0:3\], {z4\.d - z7\.d}
**	ret
*/
TEST_ZA_XN (write_za64_u64_z4_1_1, svuint64x4_t,
	    svwrite_ver_za64_u64_vg4 (1, 1, z4),
	    svwrite_ver_za64_u64_vg4 (1, 1, z4))

/*
** write_za64_f64_z28_2_w11:
**	mov	(w1[2-5]), w11
**	mova	za2v\.d\[\1, 0:3\], {z28\.d - z31\.d}
**	ret
*/
TEST_ZA_XN (write_za64_f64_z28_2_w11, svfloat64x4_t,
	    svwrite_ver_za64_f64_vg4 (2, w11, z28),
	    svwrite_ver_za64_f64_vg4 (2, w11, z28))

/*
** write_za64_s64_z0_3_w12:
**	mova	za3v\.d\[w12, 0:3\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (write_za64_s64_z0_3_w12, svint64x4_t,
	    svwrite_ver_za64_s64_vg4 (3, w12, z0),
	    svwrite_ver_za64_s64_vg4 (3, w12, z0))

/*
** write_za64_u64_z18_4_w15:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mova	za4v\.d\[w15, 0:3\], {[^\n]+}
**	ret
*/
TEST_ZA_XN (write_za64_u64_z18_4_w15, svuint64x4_t,
	    svwrite_ver_za64_u64_vg4 (4, w15, z18),
	    svwrite_ver_za64_u64_vg4 (4, w15, z18))

/*
** write_za64_f64_z23_5_w12p4:
**	add	(w[0-9]+), w12, #?4
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mova	za5v\.d\[\1, 0:3\], {[^\n]+}
**	ret
*/
TEST_ZA_XN (write_za64_f64_z23_5_w12p4, svfloat64x4_t,
	    svwrite_ver_za64_f64_vg4 (5, w12 + 4, z23),
	    svwrite_ver_za64_f64_vg4 (5, w12 + 4, z23))

/*
** write_za64_u64_z4_6_w12p1:
**	add	(w[0-9]+), w12, #?1
**	mova	za6v\.d\[\1, 0:3\], {z4\.d - z7\.d}
**	ret
*/
TEST_ZA_XN (write_za64_u64_z4_6_w12p1, svuint64x4_t,
	    svwrite_ver_za64_u64_vg4 (6, w12 + 1, z4),
	    svwrite_ver_za64_u64_vg4 (6, w12 + 1, z4))

/*
** write_za64_s64_z28_7_w12p2:
**	add	(w[0-9]+), w12, #?2
**	mova	za7v\.d\[\1, 0:3\], {z28\.d - z31\.d}
**	ret
*/
TEST_ZA_XN (write_za64_s64_z28_7_w12p2, svint64x4_t,
	    svwrite_ver_za64_s64_vg4 (7, w12 + 2, z28),
	    svwrite_ver_za64_s64_vg4 (7, w12 + 2, z28))

/*
** write_za64_f64_z0_0_w15p3:
**	add	(w[0-9]+), w15, #?3
**	mova	za0v\.d\[\1, 0:3\], {z0\.d - z3\.d}
**	ret
*/
TEST_ZA_XN (write_za64_f64_z0_0_w15p3, svfloat64x4_t,
	    svwrite_ver_za64_f64_vg4 (0, w15 + 3, z0),
	    svwrite_ver_za64_f64_vg4 (0, w15 + 3, z0))

/*
** write_za64_u64_z28_1_w12p4:
**	add	(w[0-9]+), w12, #?4
**	mova	za1v\.d\[\1, 0:3\], {z28\.d - z31\.d}
**	ret
*/
TEST_ZA_XN (write_za64_u64_z28_1_w12p4, svuint64x4_t,
	    svwrite_ver_za64_u64_vg4 (1, w12 + 4, z28),
	    svwrite_ver_za64_u64_vg4 (1, w12 + 4, z28))

/*
** write_za64_f64_z4_2_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	mova	za2v\.d\[\1, 0:3\], {z4\.d - z7\.d}
**	ret
*/
TEST_ZA_XN (write_za64_f64_z4_2_w12m1, svfloat64x4_t,
	    svwrite_ver_za64_f64_vg4 (2, w12 - 1, z4),
	    svwrite_ver_za64_f64_vg4 (2, w12 - 1, z4))

/*
** write_za64_u64_z28_3_w16:
**	mov	(w1[2-5]), w16
**	mova	za3v\.d\[\1, 0:3\], {z28\.d - z31\.d}
**	ret
*/
TEST_ZA_XN (write_za64_u64_z28_3_w16, svuint64x4_t,
	    svwrite_ver_za64_u64_vg4 (3, w16, z28),
	    svwrite_ver_za64_u64_vg4 (3, w16, z28))
