/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include "arm_neon.h"

// SIGNED VADD INTRINSICS

/*
**test_vadd_s8:
**	addp	v0\.8b, v0\.8b, v1\.8b
**	ret
*/
int8x8_t test_vadd_s8(int8x8_t v1, int8x8_t v2) {
 int8x8_t v3 = vpadd_s8(v1, v2);
 return v3;
}

/*
**test_vadd_s16:
**addp	v0\.4h, v0\.4h, v1\.4h
**ret
*/
int16x4_t test_vadd_s16(int16x4_t v1, int16x4_t v2) {
 int16x4_t v3 = vpadd_s16(v1, v2);
 return v3;
}

/*
**test_vadd_s32:
**	addp	v0\.2s, v0\.2s, v1\.2s
**	ret
*/
int32x2_t test_vadd_s32(int32x2_t v1, int32x2_t v2) {
 int32x2_t v3 = vpadd_s32(v1, v2);
 return v3;
}

/*
**test_vaddq_s8:
**...
**	addp	v0\.16b, v0\.16b, v1\.16b
**	ret
*/
int8x16_t test_vaddq_s8(int8x16_t v1, int8x16_t v2) {
 int8x16_t v3 = vpaddq_s8(v1, v2);
 return v3;
}

/*
**test_vaddq_s16:
**...
**	addp	v0\.8h, v0\.8h, v1\.8h
**	ret
*/
int16x8_t test_vaddq_s16(int16x8_t v1, int16x8_t v2) {
 int16x8_t v3 = vpaddq_s16(v1, v2);
 return v3;
}

/*
**test_vaddq_s32:
**...
**	addp	v0\.4s, v0\.4s, v1\.4s
**	ret
*/
int32x4_t test_vaddq_s32(int32x4_t v1, int32x4_t v2) {
 int32x4_t v3 = vpaddq_s32(v1, v2);
 return v3;
}

/*
**test_vaddq_s64:
**...
**	addp	v0\.2d, v0\.2d, v1\.2d
**	ret
*/
int64x2_t test_vaddq_s64(int64x2_t v1, int64x2_t v2) {
 int64x2_t v3 = vpaddq_s64(v1, v2);
 return v3;
}

/*
**test_vaddd_s64:
**...
**	addp	(d[0-9]+), v0\.2d
**	fmov	x0, \1
**	ret
*/
int64_t test_vaddd_s64(int64x2_t v1) {
 int64_t v2 = vpaddd_s64(v1);
 return v2;
}

/*
**test_vaddl_s8:
**...
**	saddlp	v0\.4h, v0\.8b
**	ret
*/
int16x4_t test_vaddl_s8(int8x8_t v1) {
 int16x4_t v2 = vpaddl_s8(v1);
 return v2;
}

/*
**test_vaddlq_s8:
**...
**	saddlp	v0\.8h, v0\.16b
**	ret
*/
int16x8_t test_vaddlq_s8(int8x16_t v1) {
 int16x8_t v2 = vpaddlq_s8(v1);
 return v2;
}
/*
**test_vaddl_s16:
**...
**	saddlp	v0\.2s, v0\.4h
**	ret
*/
int32x2_t test_vaddl_s16(int16x4_t v1) {
 int32x2_t v2 = vpaddl_s16(v1);
 return v2;
}

/*
**test_vaddlq_s16:
**...
**	saddlp	v0\.4s, v0\.8h
**	ret
*/
int32x4_t test_vaddlq_s16(int16x8_t v1) {
 int32x4_t v2 = vpaddlq_s16(v1);
 return v2;
}

/*
**test_vaddl_s32:
**...
**	saddlp	v0\.1d, v0\.2s
**	ret
*/
int64x1_t test_vaddl_s32(int32x2_t v1) {
 int64x1_t v2 = vpaddl_s32(v1);
 return v2;
}

/*
**test_vaddlq_s32:
**...
**	saddlp	v0\.2d, v0\.4s
**	ret
*/
int64x2_t test_vaddlq_s32(int32x4_t v1) {
 int64x2_t v2 = vpaddlq_s32(v1);
 return v2;
}

// UNSIGNED VADD INTRINSICS

/*
**test_vadd_u8:
**...
**	addp	v0\.8b, v0\.8b, v1\.8b
**	ret
*/
uint8x8_t test_vadd_u8(uint8x8_t v1, uint8x8_t v2) {
 uint8x8_t v3 = vpadd_u8(v1, v2);
 return v3;
}

/*
**test_vadd_u16:
**...
**	addp	v0\.4h, v0\.4h, v1\.4h
**	ret
*/
uint16x4_t test_vadd_u16(uint16x4_t v1, uint16x4_t v2) {
 uint16x4_t v3 = vpadd_u16(v1, v2);
 return v3;
}

/*
**test_vadd_u32:
**...
**	addp	v0\.2s, v0\.2s, v1\.2s
**	ret
*/
uint32x2_t test_vadd_u32(uint32x2_t v1, uint32x2_t v2) {
 uint32x2_t v3 = vpadd_u32(v1, v2);
 return v3;
}

/*
**test_vaddq_u8:
**...
**	addp	v0\.16b, v0\.16b, v1\.16b
**	ret
*/
uint8x16_t test_vaddq_u8(uint8x16_t v1, uint8x16_t v2) {
 uint8x16_t v3 = vpaddq_u8(v1, v2);
 return v3;
}

/*
**test_vaddq_u16:
**...
**	addp	v0\.8h, v0\.8h, v1\.8h
**	ret
*/
uint16x8_t test_vaddq_u16(uint16x8_t v1, uint16x8_t v2) {
 uint16x8_t v3 = vpaddq_u16(v1, v2);
 return v3;
}

/*
**test_vaddq_u32:
**...
**	addp	v0\.4s, v0\.4s, v1\.4s
**	ret
*/
uint32x4_t test_vaddq_u32(uint32x4_t v1, uint32x4_t v2) {
 uint32x4_t v3 = vpaddq_u32(v1, v2);
 return v3;
}

/*
**test_vaddq_u64:
**...
**	addp	v0\.2d, v0\.2d, v1\.2d
**	ret
*/
uint64x2_t test_vaddq_u64(uint64x2_t v1, uint64x2_t v2) {
 uint64x2_t v3 = vpaddq_u64(v1, v2);
 return v3;
}

/*
**test_vaddd_u64:
**...
**	addp	(d[0-9]+), v0\.2d
**	fmov	x0, \1
**	ret
*/
uint64_t test_vaddd_u64(uint64x2_t v1) {
 uint64_t v2 = vpaddd_u64(v1);
 return v2;
}

/*
**test_vaddl_u8:
**...
**	uaddlp	v0\.4h, v0\.8b
**	ret
*/
uint16x4_t test_vaddl_u8(uint8x8_t v1) {
 uint16x4_t v2 = vpaddl_u8(v1);
 return v2;
}

/*
**test_vaddlq_u8:
**...
**	uaddlp	v0\.8h, v0\.16b
**	ret
*/
uint16x8_t test_vaddlq_u8(uint8x16_t v1) {
 uint16x8_t v2 = vpaddlq_u8(v1);
 return v2;
}
/*
**test_vaddl_u16:
**...
**	uaddlp	v0\.2s, v0\.4h
**	ret
*/
uint32x2_t test_vaddl_u16(uint16x4_t v1) {
 uint32x2_t v2 = vpaddl_u16(v1);
 return v2;
}

/*
**test_vaddlq_u16:
**...
**	uaddlp	v0\.4s, v0\.8h
**	ret
*/
uint32x4_t test_vaddlq_u16(uint16x8_t v1) {
 uint32x4_t v2 = vpaddlq_u16(v1);
 return v2;
}

/*
**test_vaddl_u32:
**...
**	uaddlp	v0\.1d, v0\.2s
**	ret
*/
uint64x1_t test_vaddl_u32(uint32x2_t v1) {
 uint64x1_t v2 = vpaddl_u32(v1);
 return v2;
}

/*
**test_vaddlq_u32:
**...
**	uaddlp	v0\.2d, v0\.4s
**	ret
*/
uint64x2_t test_vaddlq_u32(uint32x4_t v1) {
 uint64x2_t v2 = vpaddlq_u32(v1);
 return v2;
}

// FLOATING POINT VADD INTRINSICS

/*
**test_vadd_f32:
**...
**	faddp	v0\.2s, v0\.2s, v1\.2s
**	ret
*/
float32x2_t test_vadd_f32(float32x2_t v1, float32x2_t v2) {
 float32x2_t v3 = vpadd_f32(v1, v2);
 return v3;
}

/*
**test_vaddq_f32:
**...
**	faddp	v0\.4s, v0\.4s, v1\.4s
**	ret
*/
float32x4_t test_vaddq_f32(float32x4_t v1, float32x4_t v2) {
 float32x4_t v3 = vpaddq_f32(v1, v2);
 return v3;
}

/*
**test_vaddq_f64:
**...
**	faddp	v0\.2d, v0\.2d, v1\.2d
**	ret
*/
float64x2_t test_vaddq_f64(float64x2_t v1, float64x2_t v2) {
 float64x2_t v3 = vpaddq_f64(v1, v2);
 return v3;
}

/*
**test_vadds_f32:
**...
**	faddp	s0, v0\.2s
**	ret
*/
float32_t test_vadds_f32(float32x2_t v1) {
 float32_t v2 = vpadds_f32(v1);
 return v2;
}

/*
**test_vaddd_f64:
**...
**	faddp	d0, v0\.2d
**	ret
*/
float64_t test_vaddd_f64(float64x2_t v1) {
 float64_t v2 = vpaddd_f64(v1);
 return v2;
}
