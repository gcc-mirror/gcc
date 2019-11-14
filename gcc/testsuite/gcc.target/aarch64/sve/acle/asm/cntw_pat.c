/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cntw_pow2:
**	cntw	x0, pow2
**	ret
*/
PROTO (cntw_pow2, uint64_t, ()) { return svcntw_pat (SV_POW2); }

/*
** cntw_vl1:
**	mov	x0, #?1
**	ret
*/
PROTO (cntw_vl1, uint64_t, ()) { return svcntw_pat (SV_VL1); }

/*
** cntw_vl2:
**	mov	x0, #?2
**	ret
*/
PROTO (cntw_vl2, uint64_t, ()) { return svcntw_pat (SV_VL2); }

/*
** cntw_vl3:
**	mov	x0, #?3
**	ret
*/
PROTO (cntw_vl3, uint64_t, ()) { return svcntw_pat (SV_VL3); }

/*
** cntw_vl4:
**	mov	x0, #?4
**	ret
*/
PROTO (cntw_vl4, uint64_t, ()) { return svcntw_pat (SV_VL4); }

/*
** cntw_vl5:
**	cntw	x0, vl5
**	ret
*/
PROTO (cntw_vl5, uint64_t, ()) { return svcntw_pat (SV_VL5); }

/*
** cntw_vl6:
**	cntw	x0, vl6
**	ret
*/
PROTO (cntw_vl6, uint64_t, ()) { return svcntw_pat (SV_VL6); }

/*
** cntw_vl7:
**	cntw	x0, vl7
**	ret
*/
PROTO (cntw_vl7, uint64_t, ()) { return svcntw_pat (SV_VL7); }

/*
** cntw_vl8:
**	cntw	x0, vl8
**	ret
*/
PROTO (cntw_vl8, uint64_t, ()) { return svcntw_pat (SV_VL8); }

/*
** cntw_vl16:
**	cntw	x0, vl16
**	ret
*/
PROTO (cntw_vl16, uint64_t, ()) { return svcntw_pat (SV_VL16); }

/*
** cntw_vl32:
**	cntw	x0, vl32
**	ret
*/
PROTO (cntw_vl32, uint64_t, ()) { return svcntw_pat (SV_VL32); }

/*
** cntw_vl64:
**	cntw	x0, vl64
**	ret
*/
PROTO (cntw_vl64, uint64_t, ()) { return svcntw_pat (SV_VL64); }

/*
** cntw_vl128:
**	cntw	x0, vl128
**	ret
*/
PROTO (cntw_vl128, uint64_t, ()) { return svcntw_pat (SV_VL128); }

/*
** cntw_vl256:
**	cntw	x0, vl256
**	ret
*/
PROTO (cntw_vl256, uint64_t, ()) { return svcntw_pat (SV_VL256); }

/*
** cntw_mul3:
**	cntw	x0, mul3
**	ret
*/
PROTO (cntw_mul3, uint64_t, ()) { return svcntw_pat (SV_MUL3); }

/*
** cntw_mul4:
**	cntw	x0, mul4
**	ret
*/
PROTO (cntw_mul4, uint64_t, ()) { return svcntw_pat (SV_MUL4); }

/*
** cntw_all:
**	cntw	x0
**	ret
*/
PROTO (cntw_all, uint64_t, ()) { return svcntw_pat (SV_ALL); }

/*
** incw_32_pow2:
**	incw	x0, pow2
**	ret
*/
PROTO (incw_32_pow2, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_POW2); }

/*
** incw_32_vl1:
**	add	w0, w0, #?1
**	ret
*/
PROTO (incw_32_vl1, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_VL1); }

/*
** incw_32_vl2:
**	add	w0, w0, #?2
**	ret
*/
PROTO (incw_32_vl2, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_VL2); }

/*
** incw_32_vl3:
**	add	w0, w0, #?3
**	ret
*/
PROTO (incw_32_vl3, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_VL3); }

/*
** incw_32_vl4:
**	add	w0, w0, #?4
**	ret
*/
PROTO (incw_32_vl4, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_VL4); }

/*
** incw_32_vl5:
**	incw	x0, vl5
**	ret
*/
PROTO (incw_32_vl5, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_VL5); }

/*
** incw_32_vl6:
**	incw	x0, vl6
**	ret
*/
PROTO (incw_32_vl6, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_VL6); }

/*
** incw_32_vl7:
**	incw	x0, vl7
**	ret
*/
PROTO (incw_32_vl7, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_VL7); }

/*
** incw_32_vl8:
**	incw	x0, vl8
**	ret
*/
PROTO (incw_32_vl8, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_VL8); }

/*
** incw_32_vl16:
**	incw	x0, vl16
**	ret
*/
PROTO (incw_32_vl16, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_VL16); }

/*
** incw_32_vl32:
**	incw	x0, vl32
**	ret
*/
PROTO (incw_32_vl32, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_VL32); }

/*
** incw_32_vl64:
**	incw	x0, vl64
**	ret
*/
PROTO (incw_32_vl64, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_VL64); }

/*
** incw_32_vl128:
**	incw	x0, vl128
**	ret
*/
PROTO (incw_32_vl128, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_VL128); }

/*
** incw_32_vl256:
**	incw	x0, vl256
**	ret
*/
PROTO (incw_32_vl256, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_VL256); }

/*
** incw_32_mul3:
**	incw	x0, mul3
**	ret
*/
PROTO (incw_32_mul3, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_MUL3); }

/*
** incw_32_mul4:
**	incw	x0, mul4
**	ret
*/
PROTO (incw_32_mul4, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_MUL4); }

/*
** incw_32_all:
**	incw	x0
**	ret
*/
PROTO (incw_32_all, uint32_t, (uint32_t w0)) { return w0 + svcntw_pat (SV_ALL); }

/*
** incw_64_pow2:
**	incw	x0, pow2
**	ret
*/
PROTO (incw_64_pow2, uint64_t, (uint64_t x0)) { return x0 + svcntw_pat (SV_POW2); }

/*
** incw_64_all:
**	incw	x0
**	ret
*/
PROTO (incw_64_all, uint64_t, (uint64_t x0)) { return x0 + svcntw_pat (SV_ALL); }

/*
** decw_32_pow2:
**	decw	x0, pow2
**	ret
*/
PROTO (decw_32_pow2, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_POW2); }

/*
** decw_32_vl1:
**	sub	w0, w0, #?1
**	ret
*/
PROTO (decw_32_vl1, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_VL1); }

/*
** decw_32_vl2:
**	sub	w0, w0, #?2
**	ret
*/
PROTO (decw_32_vl2, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_VL2); }

/*
** decw_32_vl3:
**	sub	w0, w0, #?3
**	ret
*/
PROTO (decw_32_vl3, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_VL3); }

/*
** decw_32_vl4:
**	sub	w0, w0, #?4
**	ret
*/
PROTO (decw_32_vl4, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_VL4); }

/*
** decw_32_vl5:
**	decw	x0, vl5
**	ret
*/
PROTO (decw_32_vl5, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_VL5); }

/*
** decw_32_vl6:
**	decw	x0, vl6
**	ret
*/
PROTO (decw_32_vl6, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_VL6); }

/*
** decw_32_vl7:
**	decw	x0, vl7
**	ret
*/
PROTO (decw_32_vl7, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_VL7); }

/*
** decw_32_vl8:
**	decw	x0, vl8
**	ret
*/
PROTO (decw_32_vl8, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_VL8); }

/*
** decw_32_vl16:
**	decw	x0, vl16
**	ret
*/
PROTO (decw_32_vl16, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_VL16); }

/*
** decw_32_vl32:
**	decw	x0, vl32
**	ret
*/
PROTO (decw_32_vl32, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_VL32); }

/*
** decw_32_vl64:
**	decw	x0, vl64
**	ret
*/
PROTO (decw_32_vl64, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_VL64); }

/*
** decw_32_vl128:
**	decw	x0, vl128
**	ret
*/
PROTO (decw_32_vl128, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_VL128); }

/*
** decw_32_vl256:
**	decw	x0, vl256
**	ret
*/
PROTO (decw_32_vl256, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_VL256); }

/*
** decw_32_mul3:
**	decw	x0, mul3
**	ret
*/
PROTO (decw_32_mul3, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_MUL3); }

/*
** decw_32_mul4:
**	decw	x0, mul4
**	ret
*/
PROTO (decw_32_mul4, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_MUL4); }

/*
** decw_32_all:
**	decw	x0
**	ret
*/
PROTO (decw_32_all, uint32_t, (uint32_t w0)) { return w0 - svcntw_pat (SV_ALL); }

/*
** decw_64_pow2:
**	decw	x0, pow2
**	ret
*/
PROTO (decw_64_pow2, uint64_t, (uint64_t x0)) { return x0 - svcntw_pat (SV_POW2); }

/*
** decw_64_all:
**	decw	x0
**	ret
*/
PROTO (decw_64_all, uint64_t, (uint64_t x0)) { return x0 - svcntw_pat (SV_ALL); }

/*
** incw_s32_pow2_z0:
**	incw	z0\.s, pow2
**	ret
*/
TEST_UNIFORM_Z (incw_s32_pow2_z0, svint32_t,
		z0 = svadd_n_s32_x (svptrue_b32 (), z0, svcntw_pat (SV_POW2)),
		z0 = svadd_x (svptrue_b32 (), z0, svcntw_pat (SV_POW2)));

/*
** incw_s32_pow2_z1:
**	movprfx	z0, z1
**	incw	z0\.s, pow2
**	ret
*/
TEST_UNIFORM_Z (incw_s32_pow2_z1, svint32_t,
		z0 = svadd_n_s32_x (svptrue_b32 (), z1, svcntw_pat (SV_POW2)),
		z0 = svadd_x (svptrue_b32 (), z1, svcntw_pat (SV_POW2)));

/*
** decw_s32_pow2_z0:
**	decw	z0\.s, pow2
**	ret
*/
TEST_UNIFORM_Z (decw_s32_pow2_z0, svint32_t,
		z0 = svsub_n_s32_x (svptrue_b32 (), z0, svcntw_pat (SV_POW2)),
		z0 = svsub_x (svptrue_b32 (), z0, svcntw_pat (SV_POW2)));

/*
** decw_s32_pow2_z1:
**	movprfx	z0, z1
**	decw	z0\.s, pow2
**	ret
*/
TEST_UNIFORM_Z (decw_s32_pow2_z1, svint32_t,
		z0 = svsub_n_s32_x (svptrue_b32 (), z1, svcntw_pat (SV_POW2)),
		z0 = svsub_x (svptrue_b32 (), z1, svcntw_pat (SV_POW2)));
