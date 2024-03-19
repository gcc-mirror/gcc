/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** read_za64_s64_z0_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	{z0\.d - z3\.d}, za0h\.d\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za64_s64_z0_0_0, svint64x4_t,
		 z0 = svread_hor_za64_s64_vg4 (0, 0),
		 z0 = svread_hor_za64_s64_vg4 (0, 0))

/*
** read_za64_u64_z4_1_1:
**	mov	(w1[2-5]), #?1
**	mova	{z4\.d - z7\.d}, za1h\.d\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za64_u64_z4_1_1, svuint64x4_t,
		 z4 = svread_hor_za64_u64_vg4 (1, 1),
		 z4 = svread_hor_za64_u64_vg4 (1, 1))

/*
** read_za64_f64_z28_2_w11:
**	mov	(w1[2-5]), w11
**	mova	{z28\.d - z31\.d}, za2h\.d\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za64_f64_z28_2_w11, svfloat64x4_t,
		 z28 = svread_hor_za64_f64_vg4 (2, w11),
		 z28 = svread_hor_za64_f64_vg4 (2, w11))

/*
** read_za64_s64_z0_3_w12:
**	mova	{z0\.d - z3\.d}, za3h\.d\[w12, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za64_s64_z0_3_w12, svint64x4_t,
		 z0 = svread_hor_za64_s64_vg4 (3, w12),
		 z0 = svread_hor_za64_s64_vg4 (3, w12))

/*
** read_za64_u64_z18_4_w15:
**	mova	{[^\n]+}, za4h\.d\[w15, 0:3\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (read_za64_u64_z18_4_w15, svuint64x4_t,
		 z18 = svread_hor_za64_u64_vg4 (4, w15),
		 z18 = svread_hor_za64_u64_vg4 (4, w15))

/*
** read_za64_f64_z23_5_w12p4:
**	add	(w[0-9]+), w12, #?4
**	mova	{[^\n]+}, za5h\.d\[\1, 0:3\]
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_READ_ZA_XN (read_za64_f64_z23_5_w12p4, svfloat64x4_t,
		 z23 = svread_hor_za64_f64_vg4 (5, w12 + 4),
		 z23 = svread_hor_za64_f64_vg4 (5, w12 + 4))

/*
** read_za64_u64_z4_6_w12p1:
**	add	(w[0-9]+), w12, #?1
**	mova	{z4\.d - z7\.d}, za6h\.d\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za64_u64_z4_6_w12p1, svuint64x4_t,
		 z4 = svread_hor_za64_u64_vg4 (6, w12 + 1),
		 z4 = svread_hor_za64_u64_vg4 (6, w12 + 1))

/*
** read_za64_s64_z28_7_w12p2:
**	add	(w[0-9]+), w12, #?2
**	mova	{z28\.d - z31\.d}, za7h\.d\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za64_s64_z28_7_w12p2, svint64x4_t,
		 z28 = svread_hor_za64_s64_vg4 (7, w12 + 2),
		 z28 = svread_hor_za64_s64_vg4 (7, w12 + 2))

/*
** read_za64_f64_z0_0_w15p3:
**	add	(w[0-9]+), w15, #?3
**	mova	{z0\.d - z3\.d}, za0h\.d\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za64_f64_z0_0_w15p3, svfloat64x4_t,
		 z0 = svread_hor_za64_f64_vg4 (0, w15 + 3),
		 z0 = svread_hor_za64_f64_vg4 (0, w15 + 3))

/*
** read_za64_u64_z28_1_w12p4:
**	add	(w[0-9]+), w12, #?4
**	mova	{z28\.d - z31\.d}, za1h\.d\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za64_u64_z28_1_w12p4, svuint64x4_t,
		 z28 = svread_hor_za64_u64_vg4 (1, w12 + 4),
		 z28 = svread_hor_za64_u64_vg4 (1, w12 + 4))

/*
** read_za64_f64_z4_2_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	mova	{z4\.d - z7\.d}, za2h\.d\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za64_f64_z4_2_w12m1, svfloat64x4_t,
		 z4 = svread_hor_za64_f64_vg4 (2, w12 - 1),
		 z4 = svread_hor_za64_f64_vg4 (2, w12 - 1))

/*
** read_za64_u64_z28_3_w16:
**	mov	(w1[2-5]), w16
**	mova	{z28\.d - z31\.d}, za3h\.d\[\1, 0:3\]
**	ret
*/
TEST_READ_ZA_XN (read_za64_u64_z28_3_w16, svuint64x4_t,
		 z28 = svread_hor_za64_u64_vg4 (3, w16),
		 z28 = svread_hor_za64_u64_vg4 (3, w16))
