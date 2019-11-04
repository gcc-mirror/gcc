/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cntd_pow2:
**	cntd	x0, pow2
**	ret
*/
PROTO (cntd_pow2, uint64_t, ()) { return svcntd_pat (SV_POW2); }

/*
** cntd_vl1:
**	mov	x0, #?1
**	ret
*/
PROTO (cntd_vl1, uint64_t, ()) { return svcntd_pat (SV_VL1); }

/*
** cntd_vl2:
**	mov	x0, #?2
**	ret
*/
PROTO (cntd_vl2, uint64_t, ()) { return svcntd_pat (SV_VL2); }

/*
** cntd_vl3:
**	cntd	x0, vl3
**	ret
*/
PROTO (cntd_vl3, uint64_t, ()) { return svcntd_pat (SV_VL3); }

/*
** cntd_vl4:
**	cntd	x0, vl4
**	ret
*/
PROTO (cntd_vl4, uint64_t, ()) { return svcntd_pat (SV_VL4); }

/*
** cntd_vl5:
**	cntd	x0, vl5
**	ret
*/
PROTO (cntd_vl5, uint64_t, ()) { return svcntd_pat (SV_VL5); }

/*
** cntd_vl6:
**	cntd	x0, vl6
**	ret
*/
PROTO (cntd_vl6, uint64_t, ()) { return svcntd_pat (SV_VL6); }

/*
** cntd_vl7:
**	cntd	x0, vl7
**	ret
*/
PROTO (cntd_vl7, uint64_t, ()) { return svcntd_pat (SV_VL7); }

/*
** cntd_vl8:
**	cntd	x0, vl8
**	ret
*/
PROTO (cntd_vl8, uint64_t, ()) { return svcntd_pat (SV_VL8); }

/*
** cntd_vl16:
**	cntd	x0, vl16
**	ret
*/
PROTO (cntd_vl16, uint64_t, ()) { return svcntd_pat (SV_VL16); }

/*
** cntd_vl32:
**	cntd	x0, vl32
**	ret
*/
PROTO (cntd_vl32, uint64_t, ()) { return svcntd_pat (SV_VL32); }

/*
** cntd_vl64:
**	cntd	x0, vl64
**	ret
*/
PROTO (cntd_vl64, uint64_t, ()) { return svcntd_pat (SV_VL64); }

/*
** cntd_vl128:
**	cntd	x0, vl128
**	ret
*/
PROTO (cntd_vl128, uint64_t, ()) { return svcntd_pat (SV_VL128); }

/*
** cntd_vl256:
**	cntd	x0, vl256
**	ret
*/
PROTO (cntd_vl256, uint64_t, ()) { return svcntd_pat (SV_VL256); }

/*
** cntd_mul3:
**	cntd	x0, mul3
**	ret
*/
PROTO (cntd_mul3, uint64_t, ()) { return svcntd_pat (SV_MUL3); }

/*
** cntd_mul4:
**	cntd	x0, mul4
**	ret
*/
PROTO (cntd_mul4, uint64_t, ()) { return svcntd_pat (SV_MUL4); }

/*
** cntd_all:
**	cntd	x0
**	ret
*/
PROTO (cntd_all, uint64_t, ()) { return svcntd_pat (SV_ALL); }

/*
** incd_32_pow2:
**	incd	x0, pow2
**	ret
*/
PROTO (incd_32_pow2, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_POW2); }

/*
** incd_32_vl1:
**	add	w0, w0, #?1
**	ret
*/
PROTO (incd_32_vl1, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_VL1); }

/*
** incd_32_vl2:
**	add	w0, w0, #?2
**	ret
*/
PROTO (incd_32_vl2, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_VL2); }

/*
** incd_32_vl3:
**	incd	x0, vl3
**	ret
*/
PROTO (incd_32_vl3, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_VL3); }

/*
** incd_32_vl4:
**	incd	x0, vl4
**	ret
*/
PROTO (incd_32_vl4, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_VL4); }

/*
** incd_32_vl5:
**	incd	x0, vl5
**	ret
*/
PROTO (incd_32_vl5, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_VL5); }

/*
** incd_32_vl6:
**	incd	x0, vl6
**	ret
*/
PROTO (incd_32_vl6, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_VL6); }

/*
** incd_32_vl7:
**	incd	x0, vl7
**	ret
*/
PROTO (incd_32_vl7, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_VL7); }

/*
** incd_32_vl8:
**	incd	x0, vl8
**	ret
*/
PROTO (incd_32_vl8, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_VL8); }

/*
** incd_32_vl16:
**	incd	x0, vl16
**	ret
*/
PROTO (incd_32_vl16, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_VL16); }

/*
** incd_32_vl32:
**	incd	x0, vl32
**	ret
*/
PROTO (incd_32_vl32, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_VL32); }

/*
** incd_32_vl64:
**	incd	x0, vl64
**	ret
*/
PROTO (incd_32_vl64, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_VL64); }

/*
** incd_32_vl128:
**	incd	x0, vl128
**	ret
*/
PROTO (incd_32_vl128, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_VL128); }

/*
** incd_32_vl256:
**	incd	x0, vl256
**	ret
*/
PROTO (incd_32_vl256, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_VL256); }

/*
** incd_32_mul3:
**	incd	x0, mul3
**	ret
*/
PROTO (incd_32_mul3, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_MUL3); }

/*
** incd_32_mul4:
**	incd	x0, mul4
**	ret
*/
PROTO (incd_32_mul4, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_MUL4); }

/*
** incd_32_all:
**	incd	x0
**	ret
*/
PROTO (incd_32_all, uint32_t, (uint32_t w0)) { return w0 + svcntd_pat (SV_ALL); }

/*
** incd_64_pow2:
**	incd	x0, pow2
**	ret
*/
PROTO (incd_64_pow2, uint64_t, (uint64_t x0)) { return x0 + svcntd_pat (SV_POW2); }

/*
** incd_64_all:
**	incd	x0
**	ret
*/
PROTO (incd_64_all, uint64_t, (uint64_t x0)) { return x0 + svcntd_pat (SV_ALL); }

/*
** decd_32_pow2:
**	decd	x0, pow2
**	ret
*/
PROTO (decd_32_pow2, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_POW2); }

/*
** decd_32_vl1:
**	sub	w0, w0, #?1
**	ret
*/
PROTO (decd_32_vl1, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_VL1); }

/*
** decd_32_vl2:
**	sub	w0, w0, #?2
**	ret
*/
PROTO (decd_32_vl2, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_VL2); }

/*
** decd_32_vl3:
**	decd	x0, vl3
**	ret
*/
PROTO (decd_32_vl3, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_VL3); }

/*
** decd_32_vl4:
**	decd	x0, vl4
**	ret
*/
PROTO (decd_32_vl4, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_VL4); }

/*
** decd_32_vl5:
**	decd	x0, vl5
**	ret
*/
PROTO (decd_32_vl5, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_VL5); }

/*
** decd_32_vl6:
**	decd	x0, vl6
**	ret
*/
PROTO (decd_32_vl6, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_VL6); }

/*
** decd_32_vl7:
**	decd	x0, vl7
**	ret
*/
PROTO (decd_32_vl7, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_VL7); }

/*
** decd_32_vl8:
**	decd	x0, vl8
**	ret
*/
PROTO (decd_32_vl8, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_VL8); }

/*
** decd_32_vl16:
**	decd	x0, vl16
**	ret
*/
PROTO (decd_32_vl16, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_VL16); }

/*
** decd_32_vl32:
**	decd	x0, vl32
**	ret
*/
PROTO (decd_32_vl32, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_VL32); }

/*
** decd_32_vl64:
**	decd	x0, vl64
**	ret
*/
PROTO (decd_32_vl64, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_VL64); }

/*
** decd_32_vl128:
**	decd	x0, vl128
**	ret
*/
PROTO (decd_32_vl128, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_VL128); }

/*
** decd_32_vl256:
**	decd	x0, vl256
**	ret
*/
PROTO (decd_32_vl256, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_VL256); }

/*
** decd_32_mul3:
**	decd	x0, mul3
**	ret
*/
PROTO (decd_32_mul3, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_MUL3); }

/*
** decd_32_mul4:
**	decd	x0, mul4
**	ret
*/
PROTO (decd_32_mul4, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_MUL4); }

/*
** decd_32_all:
**	decd	x0
**	ret
*/
PROTO (decd_32_all, uint32_t, (uint32_t w0)) { return w0 - svcntd_pat (SV_ALL); }

/*
** decd_64_pow2:
**	decd	x0, pow2
**	ret
*/
PROTO (decd_64_pow2, uint64_t, (uint64_t x0)) { return x0 - svcntd_pat (SV_POW2); }

/*
** decd_64_all:
**	decd	x0
**	ret
*/
PROTO (decd_64_all, uint64_t, (uint64_t x0)) { return x0 - svcntd_pat (SV_ALL); }

/*
** incd_s64_pow2_z0:
**	incd	z0\.d, pow2
**	ret
*/
TEST_UNIFORM_Z (incd_s64_pow2_z0, svint64_t,
		z0 = svadd_n_s64_x (svptrue_b64 (), z0, svcntd_pat (SV_POW2)),
		z0 = svadd_x (svptrue_b64 (), z0, svcntd_pat (SV_POW2)));

/*
** incd_s64_pow2_z1:
**	movprfx	z0, z1
**	incd	z0\.d, pow2
**	ret
*/
TEST_UNIFORM_Z (incd_s64_pow2_z1, svint64_t,
		z0 = svadd_n_s64_x (svptrue_b64 (), z1, svcntd_pat (SV_POW2)),
		z0 = svadd_x (svptrue_b64 (), z1, svcntd_pat (SV_POW2)));

/*
** decd_s64_pow2_z0:
**	decd	z0\.d, pow2
**	ret
*/
TEST_UNIFORM_Z (decd_s64_pow2_z0, svint64_t,
		z0 = svsub_n_s64_x (svptrue_b64 (), z0, svcntd_pat (SV_POW2)),
		z0 = svsub_x (svptrue_b64 (), z0, svcntd_pat (SV_POW2)));

/*
** decd_s64_pow2_z1:
**	movprfx	z0, z1
**	decd	z0\.d, pow2
**	ret
*/
TEST_UNIFORM_Z (decd_s64_pow2_z1, svint64_t,
		z0 = svsub_n_s64_x (svptrue_b64 (), z1, svcntd_pat (SV_POW2)),
		z0 = svsub_x (svptrue_b64 (), z1, svcntd_pat (SV_POW2)));
