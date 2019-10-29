/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cnth_pow2:
**	cnth	x0, pow2
**	ret
*/
PROTO (cnth_pow2, uint64_t, ()) { return svcnth_pat (SV_POW2); }

/*
** cnth_vl1:
**	mov	x0, #?1
**	ret
*/
PROTO (cnth_vl1, uint64_t, ()) { return svcnth_pat (SV_VL1); }

/*
** cnth_vl2:
**	mov	x0, #?2
**	ret
*/
PROTO (cnth_vl2, uint64_t, ()) { return svcnth_pat (SV_VL2); }

/*
** cnth_vl3:
**	mov	x0, #?3
**	ret
*/
PROTO (cnth_vl3, uint64_t, ()) { return svcnth_pat (SV_VL3); }

/*
** cnth_vl4:
**	mov	x0, #?4
**	ret
*/
PROTO (cnth_vl4, uint64_t, ()) { return svcnth_pat (SV_VL4); }

/*
** cnth_vl5:
**	mov	x0, #?5
**	ret
*/
PROTO (cnth_vl5, uint64_t, ()) { return svcnth_pat (SV_VL5); }

/*
** cnth_vl6:
**	mov	x0, #?6
**	ret
*/
PROTO (cnth_vl6, uint64_t, ()) { return svcnth_pat (SV_VL6); }

/*
** cnth_vl7:
**	mov	x0, #?7
**	ret
*/
PROTO (cnth_vl7, uint64_t, ()) { return svcnth_pat (SV_VL7); }

/*
** cnth_vl8:
**	mov	x0, #?8
**	ret
*/
PROTO (cnth_vl8, uint64_t, ()) { return svcnth_pat (SV_VL8); }

/*
** cnth_vl16:
**	cnth	x0, vl16
**	ret
*/
PROTO (cnth_vl16, uint64_t, ()) { return svcnth_pat (SV_VL16); }

/*
** cnth_vl32:
**	cnth	x0, vl32
**	ret
*/
PROTO (cnth_vl32, uint64_t, ()) { return svcnth_pat (SV_VL32); }

/*
** cnth_vl64:
**	cnth	x0, vl64
**	ret
*/
PROTO (cnth_vl64, uint64_t, ()) { return svcnth_pat (SV_VL64); }

/*
** cnth_vl128:
**	cnth	x0, vl128
**	ret
*/
PROTO (cnth_vl128, uint64_t, ()) { return svcnth_pat (SV_VL128); }

/*
** cnth_vl256:
**	cnth	x0, vl256
**	ret
*/
PROTO (cnth_vl256, uint64_t, ()) { return svcnth_pat (SV_VL256); }

/*
** cnth_mul3:
**	cnth	x0, mul3
**	ret
*/
PROTO (cnth_mul3, uint64_t, ()) { return svcnth_pat (SV_MUL3); }

/*
** cnth_mul4:
**	cnth	x0, mul4
**	ret
*/
PROTO (cnth_mul4, uint64_t, ()) { return svcnth_pat (SV_MUL4); }

/*
** cnth_all:
**	cnth	x0
**	ret
*/
PROTO (cnth_all, uint64_t, ()) { return svcnth_pat (SV_ALL); }

/*
** inch_32_pow2:
**	inch	x0, pow2
**	ret
*/
PROTO (inch_32_pow2, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_POW2); }

/*
** inch_32_vl1:
**	add	w0, w0, #?1
**	ret
*/
PROTO (inch_32_vl1, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_VL1); }

/*
** inch_32_vl2:
**	add	w0, w0, #?2
**	ret
*/
PROTO (inch_32_vl2, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_VL2); }

/*
** inch_32_vl3:
**	add	w0, w0, #?3
**	ret
*/
PROTO (inch_32_vl3, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_VL3); }

/*
** inch_32_vl4:
**	add	w0, w0, #?4
**	ret
*/
PROTO (inch_32_vl4, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_VL4); }

/*
** inch_32_vl5:
**	add	w0, w0, #?5
**	ret
*/
PROTO (inch_32_vl5, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_VL5); }

/*
** inch_32_vl6:
**	add	w0, w0, #?6
**	ret
*/
PROTO (inch_32_vl6, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_VL6); }

/*
** inch_32_vl7:
**	add	w0, w0, #?7
**	ret
*/
PROTO (inch_32_vl7, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_VL7); }

/*
** inch_32_vl8:
**	add	w0, w0, #?8
**	ret
*/
PROTO (inch_32_vl8, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_VL8); }

/*
** inch_32_vl16:
**	inch	x0, vl16
**	ret
*/
PROTO (inch_32_vl16, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_VL16); }

/*
** inch_32_vl32:
**	inch	x0, vl32
**	ret
*/
PROTO (inch_32_vl32, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_VL32); }

/*
** inch_32_vl64:
**	inch	x0, vl64
**	ret
*/
PROTO (inch_32_vl64, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_VL64); }

/*
** inch_32_vl128:
**	inch	x0, vl128
**	ret
*/
PROTO (inch_32_vl128, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_VL128); }

/*
** inch_32_vl256:
**	inch	x0, vl256
**	ret
*/
PROTO (inch_32_vl256, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_VL256); }

/*
** inch_32_mul3:
**	inch	x0, mul3
**	ret
*/
PROTO (inch_32_mul3, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_MUL3); }

/*
** inch_32_mul4:
**	inch	x0, mul4
**	ret
*/
PROTO (inch_32_mul4, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_MUL4); }

/*
** inch_32_all:
**	inch	x0
**	ret
*/
PROTO (inch_32_all, uint32_t, (uint32_t w0)) { return w0 + svcnth_pat (SV_ALL); }

/*
** inch_64_pow2:
**	inch	x0, pow2
**	ret
*/
PROTO (inch_64_pow2, uint64_t, (uint64_t x0)) { return x0 + svcnth_pat (SV_POW2); }

/*
** inch_64_all:
**	inch	x0
**	ret
*/
PROTO (inch_64_all, uint64_t, (uint64_t x0)) { return x0 + svcnth_pat (SV_ALL); }

/*
** dech_32_pow2:
**	dech	x0, pow2
**	ret
*/
PROTO (dech_32_pow2, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_POW2); }

/*
** dech_32_vl1:
**	sub	w0, w0, #?1
**	ret
*/
PROTO (dech_32_vl1, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_VL1); }

/*
** dech_32_vl2:
**	sub	w0, w0, #?2
**	ret
*/
PROTO (dech_32_vl2, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_VL2); }

/*
** dech_32_vl3:
**	sub	w0, w0, #?3
**	ret
*/
PROTO (dech_32_vl3, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_VL3); }

/*
** dech_32_vl4:
**	sub	w0, w0, #?4
**	ret
*/
PROTO (dech_32_vl4, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_VL4); }

/*
** dech_32_vl5:
**	sub	w0, w0, #?5
**	ret
*/
PROTO (dech_32_vl5, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_VL5); }

/*
** dech_32_vl6:
**	sub	w0, w0, #?6
**	ret
*/
PROTO (dech_32_vl6, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_VL6); }

/*
** dech_32_vl7:
**	sub	w0, w0, #?7
**	ret
*/
PROTO (dech_32_vl7, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_VL7); }

/*
** dech_32_vl8:
**	sub	w0, w0, #?8
**	ret
*/
PROTO (dech_32_vl8, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_VL8); }

/*
** dech_32_vl16:
**	dech	x0, vl16
**	ret
*/
PROTO (dech_32_vl16, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_VL16); }

/*
** dech_32_vl32:
**	dech	x0, vl32
**	ret
*/
PROTO (dech_32_vl32, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_VL32); }

/*
** dech_32_vl64:
**	dech	x0, vl64
**	ret
*/
PROTO (dech_32_vl64, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_VL64); }

/*
** dech_32_vl128:
**	dech	x0, vl128
**	ret
*/
PROTO (dech_32_vl128, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_VL128); }

/*
** dech_32_vl256:
**	dech	x0, vl256
**	ret
*/
PROTO (dech_32_vl256, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_VL256); }

/*
** dech_32_mul3:
**	dech	x0, mul3
**	ret
*/
PROTO (dech_32_mul3, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_MUL3); }

/*
** dech_32_mul4:
**	dech	x0, mul4
**	ret
*/
PROTO (dech_32_mul4, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_MUL4); }

/*
** dech_32_all:
**	dech	x0
**	ret
*/
PROTO (dech_32_all, uint32_t, (uint32_t w0)) { return w0 - svcnth_pat (SV_ALL); }

/*
** dech_64_pow2:
**	dech	x0, pow2
**	ret
*/
PROTO (dech_64_pow2, uint64_t, (uint64_t x0)) { return x0 - svcnth_pat (SV_POW2); }

/*
** dech_64_all:
**	dech	x0
**	ret
*/
PROTO (dech_64_all, uint64_t, (uint64_t x0)) { return x0 - svcnth_pat (SV_ALL); }

/*
** inch_s16_pow2_z0:
**	inch	z0\.h, pow2
**	ret
*/
TEST_UNIFORM_Z (inch_s16_pow2_z0, svint16_t,
		z0 = svadd_n_s16_x (svptrue_b16 (), z0, svcnth_pat (SV_POW2)),
		z0 = svadd_x (svptrue_b16 (), z0, svcnth_pat (SV_POW2)));

/*
** inch_s16_pow2_z1:
**	movprfx	z0, z1
**	inch	z0\.h, pow2
**	ret
*/
TEST_UNIFORM_Z (inch_s16_pow2_z1, svint16_t,
		z0 = svadd_n_s16_x (svptrue_b16 (), z1, svcnth_pat (SV_POW2)),
		z0 = svadd_x (svptrue_b16 (), z1, svcnth_pat (SV_POW2)));

/*
** dech_s16_pow2_z0:
**	dech	z0\.h, pow2
**	ret
*/
TEST_UNIFORM_Z (dech_s16_pow2_z0, svint16_t,
		z0 = svsub_n_s16_x (svptrue_b16 (), z0, svcnth_pat (SV_POW2)),
		z0 = svsub_x (svptrue_b16 (), z0, svcnth_pat (SV_POW2)));

/*
** dech_s16_pow2_z1:
**	movprfx	z0, z1
**	dech	z0\.h, pow2
**	ret
*/
TEST_UNIFORM_Z (dech_s16_pow2_z1, svint16_t,
		z0 = svsub_n_s16_x (svptrue_b16 (), z1, svcnth_pat (SV_POW2)),
		z0 = svsub_x (svptrue_b16 (), z1, svcnth_pat (SV_POW2)));
