/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** mls_0_z0_z0:
**	mov	(w8|w9|w10|w11), #?0
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z0\.b - z3\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mls_0_z0_z0, svuint8x4_t,
	    svmls_za32_u8_vg4x4 (0, z0, z0),
	    svmls_za32_vg4x4 (0, z0, z0))

/*
** mls_w0_z0_z0:
**	mov	(w8|w9|w10|w11), w0
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z0\.b - z3\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mls_w0_z0_z0, svuint8x4_t,
	    svmls_za32_u8_vg4x4 (w0, z0, z0),
	    svmls_za32_vg4x4 (w0, z0, z0))

/*
** mls_w8_z0_z4:
**	umlsll	za\.s\[w8, 0:3, vgx4\], {z0\.b - z3\.b}, {z4\.b - z7\.b}
**	ret
*/
TEST_ZA_XN (mls_w8_z0_z4, svuint8x4_t,
	    svmls_za32_u8_vg4x4 (w8, z0, z4),
	    svmls_za32_vg4x4 (w8, z0, z4))

/* Leave the assembler to check for correctness for misaligned registers.  */

/*
** mls_w8_z0_z18:
**	...
**	umlsll	za\.s\[w8, 0:3, vgx4\], {z0\.b - z3\.b}, [^\n]+
**	ret
*/
TEST_ZA_XN (mls_w8_z0_z18, svuint8x4_t,
	    svmls_za32_u8_vg4x4 (w8, z0, z18),
	    svmls_za32_vg4x4 (w8, z0, z18))

/*
** mls_w8_z18_z0:
**	...
**	umlsll	za\.s\[w8, 0:3, vgx4\], [^\n]+, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mls_w8_z18_z0, svuint8x4_t,
	    svmls_za32_u8_vg4x4 (w8, z18, z0),
	    svmls_za32_vg4x4 (w8, z18, z0))

/*
** mls_w8_z0_z23:
**	...
**	umlsll	za\.s\[w8, 0:3, vgx4\], {z0\.b - z3\.b}, [^\n]+
**	ret
*/
TEST_ZA_XN (mls_w8_z0_z23, svuint8x4_t,
	    svmls_za32_u8_vg4x4 (w8, z0, z23),
	    svmls_za32_vg4x4 (w8, z0, z23))

/*
** mls_w8_z23_z0:
**	...
**	umlsll	za\.s\[w8, 0:3, vgx4\], [^\n]+, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mls_w8_z23_z0, svuint8x4_t,
	    svmls_za32_u8_vg4x4 (w8, z23, z0),
	    svmls_za32_vg4x4 (w8, z23, z0))

/*
** mls_w8_z4_z28:
**	umlsll	za\.s\[w8, 0:3, vgx4\], {z4\.b - z7\.b}, {z28\.b - z31\.b}
**	ret
*/
TEST_ZA_XN (mls_w8_z4_z28, svuint8x4_t,
	    svmls_za32_u8_vg4x4 (w8, z4, z28),
	    svmls_za32_vg4x4 (w8, z4, z28))

/*
** mls_w8_z28_z0:
**	umlsll	za\.s\[w8, 0:3, vgx4\], {z28\.b - z31\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mls_w8_z28_z0, svuint8x4_t,
	    svmls_za32_u8_vg4x4 (w8, z28, z0),
	    svmls_za32_vg4x4 (w8, z28, z0))

/*
** mls_w8p1_z4_z0:
**	add	(w8|w9|w10|w11), w8, #?1
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z4\.b - z7\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mls_w8p1_z4_z0, svuint8x4_t,
	    svmls_za32_u8_vg4x4 (w8 + 1, z4, z0),
	    svmls_za32_vg4x4 (w8 + 1, z4, z0))

/*
** mls_w8p2_z4_z0:
**	add	(w8|w9|w10|w11), w8, #?2
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z4\.b - z7\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mls_w8p2_z4_z0, svuint8x4_t,
	    svmls_za32_u8_vg4x4 (w8 + 2, z4, z0),
	    svmls_za32_vg4x4 (w8 + 2, z4, z0))

/*
** mls_w11p4_z4_z0:
**	umlsll	za\.s\[w11, 4:7, vgx4\], {z4\.b - z7\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mls_w11p4_z4_z0, svuint8x4_t,
	    svmls_za32_u8_vg4x4 (w11 + 4, z4, z0),
	    svmls_za32_vg4x4 (w11 + 4, z4, z0))

/*
** mls_w8p7_z4_z0:
**	add	(w8|w9|w10|w11), w8, #?7
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z4\.b - z7\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mls_w8p7_z4_z0, svuint8x4_t,
	    svmls_za32_u8_vg4x4 (w8 + 7, z4, z0),
	    svmls_za32_vg4x4 (w8 + 7, z4, z0))

/*
** mls_w8p8_z4_z4:
**	add	(w8|w9|w10|w11), w8, #?8
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z4\.b - z7\.b}, {z4\.b - z7\.b}
**	ret
*/
TEST_ZA_XN (mls_w8p8_z4_z4, svuint8x4_t,
	    svmls_za32_u8_vg4x4 (w8 + 8, z4, z4),
	    svmls_za32_vg4x4 (w8 + 8, z4, z4))

/*
** mls_w8m1_z4_z0:
**	sub	(w8|w9|w10|w11), w8, #?1
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z4\.b - z7\.b}, {z0\.b - z3\.b}
**	ret
*/
TEST_ZA_XN (mls_w8m1_z4_z0, svuint8x4_t,
	    svmls_za32_u8_vg4x4 (w8 - 1, z4, z0),
	    svmls_za32_vg4x4 (w8 - 1, z4, z0))

/*
** mls_single_0_z1_z0:
**	mov	(w8|w9|w10|w11), #?0
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mls_single_0_z1_z0, svuint8x4_t, svuint8_t,
	        svmls_single_za32_u8_vg4x4 (0, z1, z0),
	        svmls_za32_vg4x4 (0, z1, z0))

/*
** mls_single_w0_z1_z0:
**	mov	(w8|w9|w10|w11), w0
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mls_single_w0_z1_z0, svuint8x4_t, svuint8_t,
	        svmls_single_za32_u8_vg4x4 (w0, z1, z0),
	        svmls_za32_vg4x4 (w0, z1, z0))

/*
** mls_single_w8_z1_z0:
**	umlsll	za\.s\[w8, 0:3, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mls_single_w8_z1_z0, svuint8x4_t, svuint8_t,
	        svmls_single_za32_u8_vg4x4 (w8, z1, z0),
	        svmls_za32_vg4x4 (w8, z1, z0))

/*
** mls_single_w8p1_z1_z0:
**	add	(w8|w9|w10|w11), w8, #?1
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mls_single_w8p1_z1_z0, svuint8x4_t, svuint8_t,
	        svmls_single_za32_u8_vg4x4 (w8 + 1, z1, z0),
	        svmls_za32_vg4x4 (w8 + 1, z1, z0))

/*
** mls_single_w8p4_z20_z0:
**	umlsll	za\.s\[w8, 4:7, vgx4\], {z20\.b - z23\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mls_single_w8p4_z20_z0, svuint8x4_t, svuint8_t,
	        svmls_single_za32_u8_vg4x4 (w8 + 4, z20, z0),
	        svmls_za32_vg4x4 (w8 + 4, z20, z0))

/*
** mls_single_w8p6_z27_z0:
**	add	(w8|w9|w10|w11), w8, #?6
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z27\.b - z30\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mls_single_w8p6_z27_z0, svuint8x4_t, svuint8_t,
	        svmls_single_za32_u8_vg4x4 (w8 + 6, z27, z0),
	        svmls_za32_vg4x4 (w8 + 6, z27, z0))

/*
** mls_single_w8p7_z1_z0:
**	add	(w8|w9|w10|w11), w8, #?7
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mls_single_w8p7_z1_z0, svuint8x4_t, svuint8_t,
	        svmls_single_za32_u8_vg4x4 (w8 + 7, z1, z0),
	        svmls_za32_vg4x4 (w8 + 7, z1, z0))

/*
** mls_single_w8p8_z1_z0:
**	add	(w8|w9|w10|w11), w8, #?8
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mls_single_w8p8_z1_z0, svuint8x4_t, svuint8_t,
	        svmls_single_za32_u8_vg4x4 (w8 + 8, z1, z0),
	        svmls_za32_vg4x4 (w8 + 8, z1, z0))

/*
** mls_single_w0m1_z1_z0:
**	sub	(w8|w9|w10|w11), w0, #?1
**	umlsll	za\.s\[\1, 0:3, vgx4\], {z1\.b - z4\.b}, z0\.b
**	ret
*/
TEST_ZA_SINGLE (mls_single_w0m1_z1_z0, svuint8x4_t, svuint8_t,
	        svmls_single_za32_u8_vg4x4 (w0 - 1, z1, z0),
	        svmls_za32_vg4x4 (w0 - 1, z1, z0))

/*
** mls_single_w8_z0_z15:
**	str	d15, \[sp, #?-16\]!
**	umlsll	za\.s\[w8, 0:3, vgx4\], {z0\.b - z3\.b}, z15\.b
**	ldr	d15, \[sp\], #?16
**	ret
*/
TEST_ZA_SINGLE_Z15 (mls_single_w8_z0_z15, svuint8x4_t, svuint8_t,
		    svmls_single_za32_u8_vg4x4 (w8, z0, z15),
		    svmls_za32_vg4x4 (w8, z0, z15))

/*
** mls_single_w8_z20_z16:
**	mov	(z[0-7]).d, z16.d
**	umlsll	za\.s\[w8, 0:3, vgx4\], {z20\.b - z23\.b}, \1\.b
**	ret
*/
TEST_ZA_SINGLE (mls_single_w8_z20_z16, svuint8x4_t, svuint8_t,
	        svmls_single_za32_u8_vg4x4 (w8, z20, z16),
	        svmls_za32_vg4x4 (w8, z20, z16))
