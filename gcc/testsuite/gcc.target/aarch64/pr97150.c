/* { dg-do compile } */

#include <arm_neon.h>

uint8_t  (*fp0)(uint8_t, int8_t)   = vqshlb_u8;
uint16_t (*fp1)(uint16_t, int16_t) = vqshlh_u16;
uint32_t (*fp2)(uint32_t, int32_t) = vqshls_u32;
uint64_t (*fp3)(uint64_t, int64_t) = vqshld_u64;
uint8_t  (*fp4)(uint8_t, int8_t)   = vqrshlb_u8;
uint16_t (*fp5)(uint16_t, int16_t) = vqrshlh_u16;
uint32_t (*fp6)(uint32_t, int32_t) = vqrshls_u32;
uint64_t (*fp7)(uint64_t, int64_t) = vqrshld_u64;
uint64_t (*fp8)(uint64_t, int64_t) = vshld_u64;

