/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_OP_VV (plus, 1, int8_t, +)
DEF_OP_VV (plus, 2, int8_t, +)
DEF_OP_VV (plus, 4, int8_t, +)
DEF_OP_VV (plus, 8, int8_t, +)
DEF_OP_VV (plus, 16, int8_t, +)
DEF_OP_VV (plus, 32, int8_t, +)
DEF_OP_VV (plus, 64, int8_t, +)
DEF_OP_VV (plus, 128, int8_t, +)
DEF_OP_VV (plus, 256, int8_t, +)
DEF_OP_VV (plus, 512, int8_t, +)
DEF_OP_VV (plus, 1024, int8_t, +)
DEF_OP_VV (plus, 2048, int8_t, +)
DEF_OP_VV (plus, 4096, int8_t, +)

DEF_OP_VV (plus, 1, int16_t, +)
DEF_OP_VV (plus, 2, int16_t, +)
DEF_OP_VV (plus, 4, int16_t, +)
DEF_OP_VV (plus, 8, int16_t, +)
DEF_OP_VV (plus, 16, int16_t, +)
DEF_OP_VV (plus, 32, int16_t, +)
DEF_OP_VV (plus, 64, int16_t, +)
DEF_OP_VV (plus, 128, int16_t, +)
DEF_OP_VV (plus, 256, int16_t, +)
DEF_OP_VV (plus, 512, int16_t, +)
DEF_OP_VV (plus, 1024, int16_t, +)
DEF_OP_VV (plus, 2048, int16_t, +)

DEF_OP_VV (plus, 1, int32_t, +)
DEF_OP_VV (plus, 2, int32_t, +)
DEF_OP_VV (plus, 4, int32_t, +)
DEF_OP_VV (plus, 8, int32_t, +)
DEF_OP_VV (plus, 16, int32_t, +)
DEF_OP_VV (plus, 32, int32_t, +)
DEF_OP_VV (plus, 64, int32_t, +)
DEF_OP_VV (plus, 128, int32_t, +)
DEF_OP_VV (plus, 256, int32_t, +)
DEF_OP_VV (plus, 512, int32_t, +)
DEF_OP_VV (plus, 1024, int32_t, +)

DEF_OP_VV (plus, 1, int64_t, +)
DEF_OP_VV (plus, 2, int64_t, +)
DEF_OP_VV (plus, 4, int64_t, +)
DEF_OP_VV (plus, 8, int64_t, +)
DEF_OP_VV (plus, 16, int64_t, +)
DEF_OP_VV (plus, 32, int64_t, +)
DEF_OP_VV (plus, 64, int64_t, +)
DEF_OP_VV (plus, 128, int64_t, +)
DEF_OP_VV (plus, 256, int64_t, +)
DEF_OP_VV (plus, 512, int64_t, +)

/* { dg-final { scan-assembler-times {vadd\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 42 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
