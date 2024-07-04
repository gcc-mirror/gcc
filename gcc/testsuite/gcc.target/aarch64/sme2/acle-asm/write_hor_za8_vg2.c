/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** write_za8_s8_z0_0_0:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	mova	za0h\.b\[\1, 0:1\], {z0\.b - z1\.b}
**	ret
*/
TEST_ZA_XN (write_za8_s8_z0_0_0, svint8x2_t,
	    svwrite_hor_za8_s8_vg2 (0, 0, z0),
	    svwrite_hor_za8_s8_vg2 (0, 0, z0))

/*
** write_za8_u8_z4_0_1:
**	mov	(w1[2-5]), #?1
**	mova	za0h\.b\[\1, 0:1\], {z4\.b - z5\.b}
**	ret
*/
TEST_ZA_XN (write_za8_u8_z4_0_1, svuint8x2_t,
	    svwrite_hor_za8_u8_vg2 (0, 1, z4),
	    svwrite_hor_za8_u8_vg2 (0, 1, z4))

/*
** write_za8_s8_z28_0_w11:
**	mov	(w1[2-5]), w11
**	mova	za0h\.b\[\1, 0:1\], {z28\.b - z29\.b}
**	ret
*/
TEST_ZA_XN (write_za8_s8_z28_0_w11, svint8x2_t,
	    svwrite_hor_za8_s8_vg2 (0, w11, z28),
	    svwrite_hor_za8_s8_vg2 (0, w11, z28))

/*
** write_za8_s8_z0_0_w12:
**	mova	za0h\.b\[w12, 0:1\], {z0\.b - z1\.b}
**	ret
*/
TEST_ZA_XN (write_za8_s8_z0_0_w12, svint8x2_t,
	    svwrite_hor_za8_s8_vg2 (0, w12, z0),
	    svwrite_hor_za8_s8_vg2 (0, w12, z0))

/*
** write_za8_u8_z18_0_w15:
**	mova	za0h\.b\[w15, 0:1\], {z18\.b - z19\.b}
**	ret
*/
TEST_ZA_XN (write_za8_u8_z18_0_w15, svuint8x2_t,
	    svwrite_hor_za8_u8_vg2 (0, w15, z18),
	    svwrite_hor_za8_u8_vg2 (0, w15, z18))

/*
** write_za8_s8_z23_0_w12p14:
**	mov	[^\n]+
**	mov	[^\n]+
**	mova	za0h\.b\[w12, 14:15\], {[^\n]+}
**	ret
*/
TEST_ZA_XN (write_za8_s8_z23_0_w12p14, svint8x2_t,
	    svwrite_hor_za8_s8_vg2 (0, w12 + 14, z23),
	    svwrite_hor_za8_s8_vg2 (0, w12 + 14, z23))

/*
** write_za8_u8_z4_0_w12p1:
**	add	(w[0-9]+), w12, #?1
**	mova	za0h\.b\[\1, 0:1\], {z4\.b - z5\.b}
**	ret
*/
TEST_ZA_XN (write_za8_u8_z4_0_w12p1, svuint8x2_t,
	    svwrite_hor_za8_u8_vg2 (0, w12 + 1, z4),
	    svwrite_hor_za8_u8_vg2 (0, w12 + 1, z4))

/*
** write_za8_s8_z28_0_w12p2:
**	mova	za0h\.b\[w12, 2:3\], {z28\.b - z29\.b}
**	ret
*/
TEST_ZA_XN (write_za8_s8_z28_0_w12p2, svint8x2_t,
	    svwrite_hor_za8_s8_vg2 (0, w12 + 2, z28),
	    svwrite_hor_za8_s8_vg2 (0, w12 + 2, z28))

/*
** write_za8_u8_z0_0_w15p3:
**	add	(w[0-9]+), w15, #?3
**	mova	za0h\.b\[\1, 0:1\], {z0\.b - z1\.b}
**	ret
*/
TEST_ZA_XN (write_za8_u8_z0_0_w15p3, svuint8x2_t,
	    svwrite_hor_za8_u8_vg2 (0, w15 + 3, z0),
	    svwrite_hor_za8_u8_vg2 (0, w15 + 3, z0))

/*
** write_za8_u8_z4_0_w15p12:
**	mova	za0h\.b\[w15, 12:13\], {z4\.b - z5\.b}
**	ret
*/
TEST_ZA_XN (write_za8_u8_z4_0_w15p12, svuint8x2_t,
	    svwrite_hor_za8_u8_vg2 (0, w15 + 12, z4),
	    svwrite_hor_za8_u8_vg2 (0, w15 + 12, z4))

/*
** write_za8_u8_z28_0_w12p15:
**	add	(w[0-9]+), w12, #?15
**	mova	za0h\.b\[\1, 0:1\], {z28\.b - z29\.b}
**	ret
*/
TEST_ZA_XN (write_za8_u8_z28_0_w12p15, svuint8x2_t,
	    svwrite_hor_za8_u8_vg2 (0, w12 + 15, z28),
	    svwrite_hor_za8_u8_vg2 (0, w12 + 15, z28))

/*
** write_za8_s8_z0_0_w15p16:
**	add	(w[0-9]+), w15, #?16
**	mova	za0h\.b\[\1, 0:1\], {z0\.b - z1\.b}
**	ret
*/
TEST_ZA_XN (write_za8_s8_z0_0_w15p16, svint8x2_t,
	    svwrite_hor_za8_s8_vg2 (0, w15 + 16, z0),
	    svwrite_hor_za8_s8_vg2 (0, w15 + 16, z0))

/*
** write_za8_u8_z4_0_w12m1:
**	sub	(w[0-9]+), w12, #?1
**	mova	za0h\.b\[\1, 0:1\], {z4\.b - z5\.b}
**	ret
*/
TEST_ZA_XN (write_za8_u8_z4_0_w12m1, svuint8x2_t,
	    svwrite_hor_za8_u8_vg2 (0, w12 - 1, z4),
	    svwrite_hor_za8_u8_vg2 (0, w12 - 1, z4))

/*
** write_za8_u8_z18_0_w16:
**	mov	(w1[2-5]), w16
**	mova	za0h\.b\[\1, 0:1\], {z18\.b - z19\.b}
**	ret
*/
TEST_ZA_XN (write_za8_u8_z18_0_w16, svuint8x2_t,
	    svwrite_hor_za8_u8_vg2 (0, w16, z18),
	    svwrite_hor_za8_u8_vg2 (0, w16, z18))
