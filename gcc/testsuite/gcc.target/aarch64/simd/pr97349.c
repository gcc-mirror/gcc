/* { dg-do compile } */

#include <arm_neon.h>

poly16x8_t  (*fp0)(poly16_t) = vdupq_n_p16;
poly64x2_t  (*fp1)(poly64_t) = vdupq_n_p64;
poly8x16_t  (*fp2)(poly8_t)  = vdupq_n_p8;
int16x8_t   (*fp3)(int16_t)  = vdupq_n_s16;
int8x16_t   (*fp4)(int8_t)   = vdupq_n_s8;
uint16x8_t  (*fp5)(uint16_t) = vdupq_n_u16;
uint8x16_t  (*fp6)(uint8_t)  = vdupq_n_u8;

