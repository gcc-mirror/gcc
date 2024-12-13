/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" { target { ! ilp32 } } } } */
/* { dg-additional-options "-march=armv8.6-a+f64mm" } */
/* { dg-require-effective-target aarch64_asm_f64mm_ok }  */

#include "test_sve_acle.h"

/*
** ld1ro_mf8_base:
**	ld1rob	z0\.b, p0/z, \[x0\]
**	ret
*/
TEST_LOAD (ld1ro_mf8_base, svmfloat8_t, mfloat8_t,
	   z0 = svld1ro_mf8 (p0, x0),
	   z0 = svld1ro (p0, x0))

/*
** ld1ro_mf8_index:
**	ld1rob	z0\.b, p0/z, \[x0, x1\]
**	ret
*/
TEST_LOAD (ld1ro_mf8_index, svmfloat8_t, mfloat8_t,
	   z0 = svld1ro_mf8 (p0, x0 + x1),
	   z0 = svld1ro (p0, x0 + x1))

/*
** ld1ro_mf8_1:
**	add	(x[0-9]+), x0, #?1
**	ld1rob	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1ro_mf8_1, svmfloat8_t, mfloat8_t,
	   z0 = svld1ro_mf8 (p0, x0 + 1),
	   z0 = svld1ro (p0, x0 + 1))

/*
** ld1ro_mf8_16:
**	add	(x[0-9]+), x0, #?16
**	ld1rob	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1ro_mf8_16, svmfloat8_t, mfloat8_t,
	   z0 = svld1ro_mf8 (p0, x0 + 16),
	   z0 = svld1ro (p0, x0 + 16))

/*
** ld1ro_mf8_256:
**	add	(x[0-9]+), x0, #?256
**	ld1rob	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1ro_mf8_256, svmfloat8_t, mfloat8_t,
	   z0 = svld1ro_mf8 (p0, x0 + 256),
	   z0 = svld1ro (p0, x0 + 256))

/*
** ld1ro_mf8_m1:
**	sub	(x[0-9]+), x0, #?1
**	ld1rob	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1ro_mf8_m1, svmfloat8_t, mfloat8_t,
	   z0 = svld1ro_mf8 (p0, x0 - 1),
	   z0 = svld1ro (p0, x0 - 1))

/*
** ld1ro_mf8_m16:
**	sub	(x[0-9]+), x0, #?16
**	ld1rob	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1ro_mf8_m16, svmfloat8_t, mfloat8_t,
	   z0 = svld1ro_mf8 (p0, x0 - 16),
	   z0 = svld1ro (p0, x0 - 16))

/*
** ld1ro_mf8_m288:
**	sub	(x[0-9]+), x0, #?288
**	ld1rob	z0\.b, p0/z, \[\1\]
**	ret
*/
TEST_LOAD (ld1ro_mf8_m288, svmfloat8_t, mfloat8_t,
	   z0 = svld1ro_mf8 (p0, x0 - 288),
	   z0 = svld1ro (p0, x0 - 288))

/*
** ld1ro_mf8_32:
**	ld1rob	z0\.b, p0/z, \[x0, #?32\]
**	ret
*/
TEST_LOAD (ld1ro_mf8_32, svmfloat8_t, mfloat8_t,
	   z0 = svld1ro_mf8 (p0, x0 + 32),
	   z0 = svld1ro (p0, x0 + 32))

/*
** ld1ro_mf8_224:
**	ld1rob	z0\.b, p0/z, \[x0, #?224\]
**	ret
*/
TEST_LOAD (ld1ro_mf8_224, svmfloat8_t, mfloat8_t,
	   z0 = svld1ro_mf8 (p0, x0 + 224),
	   z0 = svld1ro (p0, x0 + 224))

/*
** ld1ro_mf8_m32:
**	ld1rob	z0\.b, p0/z, \[x0, #?-32\]
**	ret
*/
TEST_LOAD (ld1ro_mf8_m32, svmfloat8_t, mfloat8_t,
	   z0 = svld1ro_mf8 (p0, x0 - 32),
	   z0 = svld1ro (p0, x0 - 32))

/*
** ld1ro_mf8_m256:
**	ld1rob	z0\.b, p0/z, \[x0, #?-256\]
**	ret
*/
TEST_LOAD (ld1ro_mf8_m256, svmfloat8_t, mfloat8_t,
	   z0 = svld1ro_mf8 (p0, x0 - 256),
	   z0 = svld1ro (p0, x0 - 256))

