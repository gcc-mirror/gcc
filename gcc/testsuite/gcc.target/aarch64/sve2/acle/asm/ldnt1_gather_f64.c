/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */

#include "test_sve_acle.h"

/*
** ldnt1_gather_f64_tied1:
**	ldnt1d	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_f64_tied1, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_f64 (p0, z0),
		     z0_res = svldnt1_gather_f64 (p0, z0))

/*
** ldnt1_gather_f64_untied:
**	ldnt1d	z0\.d, p0/z, \[z1\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_f64_untied, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_f64 (p0, z1),
		     z0_res = svldnt1_gather_f64 (p0, z1))

/*
** ldnt1_gather_x0_f64_offset:
**	ldnt1d	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_x0_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_offset_f64 (p0, z0, x0),
		     z0_res = svldnt1_gather_offset_f64 (p0, z0, x0))

/*
** ldnt1_gather_m8_f64_offset:
**	mov	(x[0-9]+), #?-8
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_m8_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_offset_f64 (p0, z0, -8),
		     z0_res = svldnt1_gather_offset_f64 (p0, z0, -8))

/*
** ldnt1_gather_0_f64_offset:
**	ldnt1d	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_0_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_offset_f64 (p0, z0, 0),
		     z0_res = svldnt1_gather_offset_f64 (p0, z0, 0))

/*
** ldnt1_gather_9_f64_offset:
**	mov	(x[0-9]+), #?9
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_9_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_offset_f64 (p0, z0, 9),
		     z0_res = svldnt1_gather_offset_f64 (p0, z0, 9))

/*
** ldnt1_gather_10_f64_offset:
**	mov	(x[0-9]+), #?10
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_10_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_offset_f64 (p0, z0, 10),
		     z0_res = svldnt1_gather_offset_f64 (p0, z0, 10))

/*
** ldnt1_gather_11_f64_offset:
**	mov	(x[0-9]+), #?11
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_11_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_offset_f64 (p0, z0, 11),
		     z0_res = svldnt1_gather_offset_f64 (p0, z0, 11))

/*
** ldnt1_gather_12_f64_offset:
**	mov	(x[0-9]+), #?12
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_12_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_offset_f64 (p0, z0, 12),
		     z0_res = svldnt1_gather_offset_f64 (p0, z0, 12))

/*
** ldnt1_gather_13_f64_offset:
**	mov	(x[0-9]+), #?13
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_13_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_offset_f64 (p0, z0, 13),
		     z0_res = svldnt1_gather_offset_f64 (p0, z0, 13))

/*
** ldnt1_gather_14_f64_offset:
**	mov	(x[0-9]+), #?14
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_14_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_offset_f64 (p0, z0, 14),
		     z0_res = svldnt1_gather_offset_f64 (p0, z0, 14))

/*
** ldnt1_gather_15_f64_offset:
**	mov	(x[0-9]+), #?15
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_15_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_offset_f64 (p0, z0, 15),
		     z0_res = svldnt1_gather_offset_f64 (p0, z0, 15))

/*
** ldnt1_gather_16_f64_offset:
**	mov	(x[0-9]+), #?16
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_16_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_offset_f64 (p0, z0, 16),
		     z0_res = svldnt1_gather_offset_f64 (p0, z0, 16))

/*
** ldnt1_gather_248_f64_offset:
**	mov	(x[0-9]+), #?248
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_248_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_offset_f64 (p0, z0, 248),
		     z0_res = svldnt1_gather_offset_f64 (p0, z0, 248))

/*
** ldnt1_gather_256_f64_offset:
**	mov	(x[0-9]+), #?256
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_256_f64_offset, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_offset_f64 (p0, z0, 256),
		     z0_res = svldnt1_gather_offset_f64 (p0, z0, 256))

/*
** ldnt1_gather_x0_f64_index:
**	lsl	(x[0-9]+), x0, #?3
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_x0_f64_index, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_index_f64 (p0, z0, x0),
		     z0_res = svldnt1_gather_index_f64 (p0, z0, x0))

/*
** ldnt1_gather_m1_f64_index:
**	mov	(x[0-9]+), #?-8
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_m1_f64_index, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_index_f64 (p0, z0, -1),
		     z0_res = svldnt1_gather_index_f64 (p0, z0, -1))

/*
** ldnt1_gather_0_f64_index:
**	ldnt1d	z0\.d, p0/z, \[z0\.d\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_0_f64_index, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_index_f64 (p0, z0, 0),
		     z0_res = svldnt1_gather_index_f64 (p0, z0, 0))

/*
** ldnt1_gather_5_f64_index:
**	mov	(x[0-9]+), #?40
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_5_f64_index, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_index_f64 (p0, z0, 5),
		     z0_res = svldnt1_gather_index_f64 (p0, z0, 5))

/*
** ldnt1_gather_31_f64_index:
**	mov	(x[0-9]+), #?248
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_31_f64_index, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_index_f64 (p0, z0, 31),
		     z0_res = svldnt1_gather_index_f64 (p0, z0, 31))

/*
** ldnt1_gather_32_f64_index:
**	mov	(x[0-9]+), #?256
**	ldnt1d	z0\.d, p0/z, \[z0\.d, \1\]
**	ret
*/
TEST_LOAD_GATHER_ZS (ldnt1_gather_32_f64_index, svfloat64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64base_index_f64 (p0, z0, 32),
		     z0_res = svldnt1_gather_index_f64 (p0, z0, 32))

/*
** ldnt1_gather_x0_f64_s64offset:
**	ldnt1d	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1_gather_x0_f64_s64offset, svfloat64_t, float64_t, svint64_t,
		     z0_res = svldnt1_gather_s64offset_f64 (p0, x0, z0),
		     z0_res = svldnt1_gather_offset (p0, x0, z0))

/*
** ldnt1_gather_tied1_f64_s64offset:
**	ldnt1d	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1_gather_tied1_f64_s64offset, svfloat64_t, float64_t, svint64_t,
		     z0_res = svldnt1_gather_s64offset_f64 (p0, x0, z0),
		     z0_res = svldnt1_gather_offset (p0, x0, z0))

/*
** ldnt1_gather_untied_f64_s64offset:
**	ldnt1d	z0\.d, p0/z, \[z1\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1_gather_untied_f64_s64offset, svfloat64_t, float64_t, svint64_t,
		     z0_res = svldnt1_gather_s64offset_f64 (p0, x0, z1),
		     z0_res = svldnt1_gather_offset (p0, x0, z1))

/*
** ldnt1_gather_x0_f64_u64offset:
**	ldnt1d	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1_gather_x0_f64_u64offset, svfloat64_t, float64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64offset_f64 (p0, x0, z0),
		     z0_res = svldnt1_gather_offset (p0, x0, z0))

/*
** ldnt1_gather_tied1_f64_u64offset:
**	ldnt1d	z0\.d, p0/z, \[z0\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1_gather_tied1_f64_u64offset, svfloat64_t, float64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64offset_f64 (p0, x0, z0),
		     z0_res = svldnt1_gather_offset (p0, x0, z0))

/*
** ldnt1_gather_untied_f64_u64offset:
**	ldnt1d	z0\.d, p0/z, \[z1\.d, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1_gather_untied_f64_u64offset, svfloat64_t, float64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64offset_f64 (p0, x0, z1),
		     z0_res = svldnt1_gather_offset (p0, x0, z1))

/*
** ldnt1_gather_x0_f64_s64index:
**	lsl	(z[0-9]+\.d), z0\.d, #3
**	ldnt1d	z0\.d, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1_gather_x0_f64_s64index, svfloat64_t, float64_t, svint64_t,
		     z0_res = svldnt1_gather_s64index_f64 (p0, x0, z0),
		     z0_res = svldnt1_gather_index (p0, x0, z0))

/*
** ldnt1_gather_tied1_f64_s64index:
**	lsl	(z[0-9]+\.d), z0\.d, #3
**	ldnt1d	z0\.d, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1_gather_tied1_f64_s64index, svfloat64_t, float64_t, svint64_t,
		     z0_res = svldnt1_gather_s64index_f64 (p0, x0, z0),
		     z0_res = svldnt1_gather_index (p0, x0, z0))

/*
** ldnt1_gather_untied_f64_s64index:
**	lsl	(z[0-9]+\.d), z1\.d, #3
**	ldnt1d	z0\.d, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1_gather_untied_f64_s64index, svfloat64_t, float64_t, svint64_t,
		     z0_res = svldnt1_gather_s64index_f64 (p0, x0, z1),
		     z0_res = svldnt1_gather_index (p0, x0, z1))

/*
** ldnt1_gather_x0_f64_u64index:
**	lsl	(z[0-9]+\.d), z0\.d, #3
**	ldnt1d	z0\.d, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1_gather_x0_f64_u64index, svfloat64_t, float64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64index_f64 (p0, x0, z0),
		     z0_res = svldnt1_gather_index (p0, x0, z0))

/*
** ldnt1_gather_tied1_f64_u64index:
**	lsl	(z[0-9]+\.d), z0\.d, #3
**	ldnt1d	z0\.d, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1_gather_tied1_f64_u64index, svfloat64_t, float64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64index_f64 (p0, x0, z0),
		     z0_res = svldnt1_gather_index (p0, x0, z0))

/*
** ldnt1_gather_untied_f64_u64index:
**	lsl	(z[0-9]+\.d), z1\.d, #3
**	ldnt1d	z0\.d, p0/z, \[\1, x0\]
**	ret
*/
TEST_LOAD_GATHER_SZ (ldnt1_gather_untied_f64_u64index, svfloat64_t, float64_t, svuint64_t,
		     z0_res = svldnt1_gather_u64index_f64 (p0, x0, z1),
		     z0_res = svldnt1_gather_index (p0, x0, z1))
