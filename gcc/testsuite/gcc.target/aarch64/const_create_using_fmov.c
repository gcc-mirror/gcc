/* { dg-do compile } */
/* { dg-additional-options "-march=armv9-a -Ofast" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

/*
** g:
** 	fmov	v0\.4s, 1\.0e\+0
** 	ret
*/
float32x4_t g(){
    return vdupq_n_f32(1);
}

/*
** h:
** 	fmov	v0\.4s, 1\.0e\+0
** 	ret
*/
uint32x4_t h() {
    return vreinterpretq_u32_f32(g());
}

/*
** f1:
** 	fmov	v0\.4s, 1\.0e\+0
** 	ret
*/
uint32x4_t f1() {
    return vdupq_n_u32(0x3f800000);
}

/*
** f2:
** 	fmov	v0\.4s, 1\.5e\+0
** 	ret
*/
uint32x4_t f2() {
    return vdupq_n_u32(0x3FC00000);
}

/*
** f3:
** 	fmov	v0\.4s, 1\.25e\+0
** 	ret
*/
uint32x4_t f3() {
    return vdupq_n_u32(0x3FA00000);
}

/*
** f4:
** 	fmov	v0\.2d, 1\.0e\+0
** 	ret
*/
uint64x2_t f4() {
    return vdupq_n_u64(0x3FF0000000000000);
}

/*
** fn4:
** 	fmov	v0\.2d, -1\.0e\+0
** 	ret
*/
uint64x2_t fn4() {
    return vdupq_n_u64(0xBFF0000000000000);
}

/*
** f5:
** 	fmov	v0\.8h, 1\.5e\+0
** 	ret
*/
uint16x8_t f5() {
    return vdupq_n_u16(0x3E00);
}

/*
** f6:
** 	adrp	x0, \.LC0
** 	ldr	q0, \[x0, #:lo12:\.LC0\]
** 	ret
*/
uint32x4_t f6() {
    return vdupq_n_u32(0x4f800000);
}
