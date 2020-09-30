/* { dg-do compile } */

#include <arm_neon.h>

uint32_t (*fp3)(int64_t) = vqmovund_s64;
uint8_t (*fp4)(int16_t) = vqmovunh_s16;
uint16_t (*fp5)(int32_t) = vqmovuns_s32;

