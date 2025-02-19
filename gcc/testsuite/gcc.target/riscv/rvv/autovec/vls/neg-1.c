/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_OP_V (neg, 1, int8_t, -)
DEF_OP_V (neg, 2, int8_t, -)
DEF_OP_V (neg, 4, int8_t, -)
DEF_OP_V (neg, 8, int8_t, -)
DEF_OP_V (neg, 16, int8_t, -)
DEF_OP_V (neg, 32, int8_t, -)
DEF_OP_V (neg, 64, int8_t, -)
DEF_OP_V (neg, 128, int8_t, -)
DEF_OP_V (neg, 256, int8_t, -)
DEF_OP_V (neg, 512, int8_t, -)
DEF_OP_V (neg, 1024, int8_t, -)
DEF_OP_V (neg, 2048, int8_t, -)
DEF_OP_V (neg, 4096, int8_t, -)

DEF_OP_V (neg, 1, int16_t, -)
DEF_OP_V (neg, 2, int16_t, -)
DEF_OP_V (neg, 4, int16_t, -)
DEF_OP_V (neg, 8, int16_t, -)
DEF_OP_V (neg, 16, int16_t, -)
DEF_OP_V (neg, 32, int16_t, -)
DEF_OP_V (neg, 64, int16_t, -)
DEF_OP_V (neg, 128, int16_t, -)
DEF_OP_V (neg, 256, int16_t, -)
DEF_OP_V (neg, 512, int16_t, -)
DEF_OP_V (neg, 1024, int16_t, -)
DEF_OP_V (neg, 2048, int16_t, -)

DEF_OP_V (neg, 1, int32_t, -)
DEF_OP_V (neg, 2, int32_t, -)
DEF_OP_V (neg, 4, int32_t, -)
DEF_OP_V (neg, 8, int32_t, -)
DEF_OP_V (neg, 16, int32_t, -)
DEF_OP_V (neg, 32, int32_t, -)
DEF_OP_V (neg, 64, int32_t, -)
DEF_OP_V (neg, 128, int32_t, -)
DEF_OP_V (neg, 256, int32_t, -)
DEF_OP_V (neg, 512, int32_t, -)
DEF_OP_V (neg, 1024, int32_t, -)

DEF_OP_V (neg, 1, int64_t, -)
DEF_OP_V (neg, 2, int64_t, -)
DEF_OP_V (neg, 4, int64_t, -)
DEF_OP_V (neg, 8, int64_t, -)
DEF_OP_V (neg, 16, int64_t, -)
DEF_OP_V (neg, 32, int64_t, -)
DEF_OP_V (neg, 64, int64_t, -)
DEF_OP_V (neg, 128, int64_t, -)
DEF_OP_V (neg, 256, int64_t, -)
DEF_OP_V (neg, 512, int64_t, -)

/* { dg-final { scan-assembler-times {vrsub\.vi\s+v[0-9]+,\s*v[0-9]+,\s*0} 42 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
