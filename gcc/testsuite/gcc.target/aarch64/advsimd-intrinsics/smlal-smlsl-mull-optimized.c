/* { dg-do compile { target aarch64-*-* } } */

#include <arm_neon.h>

/*
**add:
**     smlal   v0.4s, v1.4h, v2.h[3]
**     ret
*/

int32x4_t add(int32x4_t acc, int16x4_t b, int16x4_t c) {
    return vmlal_n_s16(acc, b, c[3]);
}

/*
**sub:
**     smlsl   v0.4s, v1.4h, v2.h[3]
**     ret
*/

int32x4_t sub(int32x4_t acc, int16x4_t b, int16x4_t c) {
    return vmlsl_n_s16(acc, b, c[3]);
}

/*
**smull:
**     smull   v0.4s, v1.4h, v2.h[3]
**     ret
*/

int32x4_t smull(int16x4_t b, int16x4_t c) {
    return vmull_n_s16(b, c[3]);
}

/*
**umull:
**     umull   v0.4s, v1.4h, v2.h[3]
**     ret
*/

uint32x4_t umull(uint16x4_t b, uint16x4_t c) {
    return vmull_n_u16(b, c[3]);
}

/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" {-O[^0]} } } */
