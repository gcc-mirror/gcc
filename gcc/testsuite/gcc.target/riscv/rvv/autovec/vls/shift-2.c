/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_OP_VV (shift, 1, uint8_t, >>)
DEF_OP_VV (shift, 2, uint8_t, >>)
DEF_OP_VV (shift, 4, uint8_t, >>)
DEF_OP_VV (shift, 8, uint8_t, >>)
DEF_OP_VV (shift, 16, uint8_t, >>)
DEF_OP_VV (shift, 32, uint8_t, >>)
DEF_OP_VV (shift, 64, uint8_t, >>)
DEF_OP_VV (shift, 128, uint8_t, >>)
DEF_OP_VV (shift, 256, uint8_t, >>)
DEF_OP_VV (shift, 512, uint8_t, >>)
DEF_OP_VV (shift, 1024, uint8_t, >>)
DEF_OP_VV (shift, 2048, uint8_t, >>)
DEF_OP_VV (shift, 4096, uint8_t, >>)

DEF_OP_VV (shift, 1, uint16_t, >>)
DEF_OP_VV (shift, 2, uint16_t, >>)
DEF_OP_VV (shift, 4, uint16_t, >>)
DEF_OP_VV (shift, 8, uint16_t, >>)
DEF_OP_VV (shift, 16, uint16_t, >>)
DEF_OP_VV (shift, 32, uint16_t, >>)
DEF_OP_VV (shift, 64, uint16_t, >>)
DEF_OP_VV (shift, 128, uint16_t, >>)
DEF_OP_VV (shift, 256, uint16_t, >>)
DEF_OP_VV (shift, 512, uint16_t, >>)
DEF_OP_VV (shift, 1024, uint16_t, >>)
DEF_OP_VV (shift, 2048, uint16_t, >>)

DEF_OP_VV (shift, 1, uint32_t, >>)
DEF_OP_VV (shift, 2, uint32_t, >>)
DEF_OP_VV (shift, 4, uint32_t, >>)
DEF_OP_VV (shift, 8, uint32_t, >>)
DEF_OP_VV (shift, 16, uint32_t, >>)
DEF_OP_VV (shift, 32, uint32_t, >>)
DEF_OP_VV (shift, 64, uint32_t, >>)
DEF_OP_VV (shift, 128, uint32_t, >>)
DEF_OP_VV (shift, 256, uint32_t, >>)
DEF_OP_VV (shift, 512, uint32_t, >>)
DEF_OP_VV (shift, 1024, uint32_t, >>)

DEF_OP_VV (shift, 1, uint64_t, >>)
DEF_OP_VV (shift, 2, uint64_t, >>)
DEF_OP_VV (shift, 4, uint64_t, >>)
DEF_OP_VV (shift, 8, uint64_t, >>)
DEF_OP_VV (shift, 16, uint64_t, >>)
DEF_OP_VV (shift, 32, uint64_t, >>)
DEF_OP_VV (shift, 64, uint64_t, >>)
DEF_OP_VV (shift, 128, uint64_t, >>)
DEF_OP_VV (shift, 256, uint64_t, >>)
DEF_OP_VV (shift, 512, uint64_t, >>)

/* { dg-final { scan-assembler-times {vsrl\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 19 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
