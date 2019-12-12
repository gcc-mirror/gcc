/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cntb_pow2:
**	cntb	x0, pow2
**	ret
*/
PROTO (cntb_pow2, uint64_t, ()) { return svcntb_pat (SV_POW2); }

/*
** cntb_vl1:
**	mov	x0, #?1
**	ret
*/
PROTO (cntb_vl1, uint64_t, ()) { return svcntb_pat (SV_VL1); }

/*
** cntb_vl2:
**	mov	x0, #?2
**	ret
*/
PROTO (cntb_vl2, uint64_t, ()) { return svcntb_pat (SV_VL2); }

/*
** cntb_vl3:
**	mov	x0, #?3
**	ret
*/
PROTO (cntb_vl3, uint64_t, ()) { return svcntb_pat (SV_VL3); }

/*
** cntb_vl4:
**	mov	x0, #?4
**	ret
*/
PROTO (cntb_vl4, uint64_t, ()) { return svcntb_pat (SV_VL4); }

/*
** cntb_vl5:
**	mov	x0, #?5
**	ret
*/
PROTO (cntb_vl5, uint64_t, ()) { return svcntb_pat (SV_VL5); }

/*
** cntb_vl6:
**	mov	x0, #?6
**	ret
*/
PROTO (cntb_vl6, uint64_t, ()) { return svcntb_pat (SV_VL6); }

/*
** cntb_vl7:
**	mov	x0, #?7
**	ret
*/
PROTO (cntb_vl7, uint64_t, ()) { return svcntb_pat (SV_VL7); }

/*
** cntb_vl8:
**	mov	x0, #?8
**	ret
*/
PROTO (cntb_vl8, uint64_t, ()) { return svcntb_pat (SV_VL8); }

/*
** cntb_vl16:
**	mov	x0, #?16
**	ret
*/
PROTO (cntb_vl16, uint64_t, ()) { return svcntb_pat (SV_VL16); }

/*
** cntb_vl32:
**	cntb	x0, vl32
**	ret
*/
PROTO (cntb_vl32, uint64_t, ()) { return svcntb_pat (SV_VL32); }

/*
** cntb_vl64:
**	cntb	x0, vl64
**	ret
*/
PROTO (cntb_vl64, uint64_t, ()) { return svcntb_pat (SV_VL64); }

/*
** cntb_vl128:
**	cntb	x0, vl128
**	ret
*/
PROTO (cntb_vl128, uint64_t, ()) { return svcntb_pat (SV_VL128); }

/*
** cntb_vl256:
**	cntb	x0, vl256
**	ret
*/
PROTO (cntb_vl256, uint64_t, ()) { return svcntb_pat (SV_VL256); }

/*
** cntb_mul3:
**	cntb	x0, mul3
**	ret
*/
PROTO (cntb_mul3, uint64_t, ()) { return svcntb_pat (SV_MUL3); }

/*
** cntb_mul4:
**	cntb	x0, mul4
**	ret
*/
PROTO (cntb_mul4, uint64_t, ()) { return svcntb_pat (SV_MUL4); }

/*
** cntb_all:
**	cntb	x0
**	ret
*/
PROTO (cntb_all, uint64_t, ()) { return svcntb_pat (SV_ALL); }

/*
** incb_32_pow2:
**	incb	x0, pow2
**	ret
*/
PROTO (incb_32_pow2, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_POW2); }

/*
** incb_32_vl1:
**	add	w0, w0, #?1
**	ret
*/
PROTO (incb_32_vl1, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_VL1); }

/*
** incb_32_vl2:
**	add	w0, w0, #?2
**	ret
*/
PROTO (incb_32_vl2, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_VL2); }

/*
** incb_32_vl3:
**	add	w0, w0, #?3
**	ret
*/
PROTO (incb_32_vl3, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_VL3); }

/*
** incb_32_vl4:
**	add	w0, w0, #?4
**	ret
*/
PROTO (incb_32_vl4, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_VL4); }

/*
** incb_32_vl5:
**	add	w0, w0, #?5
**	ret
*/
PROTO (incb_32_vl5, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_VL5); }

/*
** incb_32_vl6:
**	add	w0, w0, #?6
**	ret
*/
PROTO (incb_32_vl6, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_VL6); }

/*
** incb_32_vl7:
**	add	w0, w0, #?7
**	ret
*/
PROTO (incb_32_vl7, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_VL7); }

/*
** incb_32_vl8:
**	add	w0, w0, #?8
**	ret
*/
PROTO (incb_32_vl8, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_VL8); }

/*
** incb_32_vl16:
**	add	w0, w0, #?16
**	ret
*/
PROTO (incb_32_vl16, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_VL16); }

/*
** incb_32_vl32:
**	incb	x0, vl32
**	ret
*/
PROTO (incb_32_vl32, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_VL32); }

/*
** incb_32_vl64:
**	incb	x0, vl64
**	ret
*/
PROTO (incb_32_vl64, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_VL64); }

/*
** incb_32_vl128:
**	incb	x0, vl128
**	ret
*/
PROTO (incb_32_vl128, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_VL128); }

/*
** incb_32_vl256:
**	incb	x0, vl256
**	ret
*/
PROTO (incb_32_vl256, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_VL256); }

/*
** incb_32_mul3:
**	incb	x0, mul3
**	ret
*/
PROTO (incb_32_mul3, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_MUL3); }

/*
** incb_32_mul4:
**	incb	x0, mul4
**	ret
*/
PROTO (incb_32_mul4, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_MUL4); }

/*
** incb_32_all:
**	incb	x0
**	ret
*/
PROTO (incb_32_all, uint32_t, (uint32_t w0)) { return w0 + svcntb_pat (SV_ALL); }

/*
** incb_64_pow2:
**	incb	x0, pow2
**	ret
*/
PROTO (incb_64_pow2, uint64_t, (uint64_t x0)) { return x0 + svcntb_pat (SV_POW2); }

/*
** incb_64_all:
**	incb	x0
**	ret
*/
PROTO (incb_64_all, uint64_t, (uint64_t x0)) { return x0 + svcntb_pat (SV_ALL); }

/*
** decb_32_pow2:
**	decb	x0, pow2
**	ret
*/
PROTO (decb_32_pow2, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_POW2); }

/*
** decb_32_vl1:
**	sub	w0, w0, #?1
**	ret
*/
PROTO (decb_32_vl1, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_VL1); }

/*
** decb_32_vl2:
**	sub	w0, w0, #?2
**	ret
*/
PROTO (decb_32_vl2, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_VL2); }

/*
** decb_32_vl3:
**	sub	w0, w0, #?3
**	ret
*/
PROTO (decb_32_vl3, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_VL3); }

/*
** decb_32_vl4:
**	sub	w0, w0, #?4
**	ret
*/
PROTO (decb_32_vl4, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_VL4); }

/*
** decb_32_vl5:
**	sub	w0, w0, #?5
**	ret
*/
PROTO (decb_32_vl5, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_VL5); }

/*
** decb_32_vl6:
**	sub	w0, w0, #?6
**	ret
*/
PROTO (decb_32_vl6, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_VL6); }

/*
** decb_32_vl7:
**	sub	w0, w0, #?7
**	ret
*/
PROTO (decb_32_vl7, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_VL7); }

/*
** decb_32_vl8:
**	sub	w0, w0, #?8
**	ret
*/
PROTO (decb_32_vl8, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_VL8); }

/*
** decb_32_vl16:
**	sub	w0, w0, #?16
**	ret
*/
PROTO (decb_32_vl16, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_VL16); }

/*
** decb_32_vl32:
**	decb	x0, vl32
**	ret
*/
PROTO (decb_32_vl32, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_VL32); }

/*
** decb_32_vl64:
**	decb	x0, vl64
**	ret
*/
PROTO (decb_32_vl64, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_VL64); }

/*
** decb_32_vl128:
**	decb	x0, vl128
**	ret
*/
PROTO (decb_32_vl128, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_VL128); }

/*
** decb_32_vl256:
**	decb	x0, vl256
**	ret
*/
PROTO (decb_32_vl256, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_VL256); }

/*
** decb_32_mul3:
**	decb	x0, mul3
**	ret
*/
PROTO (decb_32_mul3, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_MUL3); }

/*
** decb_32_mul4:
**	decb	x0, mul4
**	ret
*/
PROTO (decb_32_mul4, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_MUL4); }

/*
** decb_32_all:
**	decb	x0
**	ret
*/
PROTO (decb_32_all, uint32_t, (uint32_t w0)) { return w0 - svcntb_pat (SV_ALL); }

/*
** decb_64_pow2:
**	decb	x0, pow2
**	ret
*/
PROTO (decb_64_pow2, uint64_t, (uint64_t x0)) { return x0 - svcntb_pat (SV_POW2); }

/*
** decb_64_all:
**	decb	x0
**	ret
*/
PROTO (decb_64_all, uint64_t, (uint64_t x0)) { return x0 - svcntb_pat (SV_ALL); }

/*
** incb_s8_pow2_z0:
**	cntb	x([0-9]+), pow2
**	mov	(z[0-9]+\.b), w\1
**	add	z0\.b, (z0\.b, \2|\2, z0\.b)
**	ret
*/
TEST_UNIFORM_Z (incb_s8_pow2_z0, svint8_t,
		z0 = svadd_n_s8_x (svptrue_b8 (), z0, svcntb_pat (SV_POW2)),
		z0 = svadd_x (svptrue_b8 (), z0, svcntb_pat (SV_POW2)));

/*
** incb_s8_pow2_z1:
**	cntb	x([0-9]+), pow2
**	mov	(z[0-9]+\.b), w\1
**	add	z0\.b, (z1\.b, \2|\2, z1\.b)
**	ret
*/
TEST_UNIFORM_Z (incb_s8_pow2_z1, svint8_t,
		z0 = svadd_n_s8_x (svptrue_b8 (), z1, svcntb_pat (SV_POW2)),
		z0 = svadd_x (svptrue_b8 (), z1, svcntb_pat (SV_POW2)));

/*
** decb_s8_pow2_z0:
**	cntb	x([0-9]+), pow2
**	mov	(z[0-9]+\.b), w\1
**	sub	z0\.b, z0\.b, \2
**	ret
*/
TEST_UNIFORM_Z (decb_s8_pow2_z0, svint8_t,
		z0 = svsub_n_s8_x (svptrue_b8 (), z0, svcntb_pat (SV_POW2)),
		z0 = svsub_x (svptrue_b8 (), z0, svcntb_pat (SV_POW2)));

/*
** decb_s8_pow2_z1:
**	cntb	x([0-9]+), pow2
**	mov	(z[0-9]+\.b), w\1
**	sub	z0\.b, z1\.b, \2
**	ret
*/
TEST_UNIFORM_Z (decb_s8_pow2_z1, svint8_t,
		z0 = svsub_n_s8_x (svptrue_b8 (), z1, svcntb_pat (SV_POW2)),
		z0 = svsub_x (svptrue_b8 (), z1, svcntb_pat (SV_POW2)));
