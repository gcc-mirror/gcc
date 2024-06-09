/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8 -fno-builtin" } */

#include "def.h"

DEF_SERIES (int16_t, 1, -1, 2, b1sm1n2)
DEF_SERIES (int16_t, 3, -1, 4, b3sm1n4)
DEF_SERIES (int16_t, 7, -1, 8, b7sm1n8)
DEF_SERIES (int16_t, 15, -1, 16, b15sm1n16)
DEF_SERIES (int16_t, 31, -1, 32, b31sm1n32)
DEF_SERIES (int16_t, 63, -1, 64, b63sm1n64)
DEF_SERIES (int16_t, 127, -1, 128, b127sm1n128)
DEF_SERIES (int16_t, 255, -1, 256, b255sm1n256)
DEF_SERIES (int16_t, 511, -1, 512, b511sm1n512)
DEF_SERIES (int16_t, 1023, -1, 1024, b1023sm1n1024)
DEF_SERIES (int16_t, 2047, -1, 2048, b2047sm1n2048)

DEF_SERIES (int32_t, 1, -1, 2, b0sm1n2)
DEF_SERIES (int32_t, 3, -1, 4, b0sm1n4)
DEF_SERIES (int32_t, 7, -1, 8, b0sm1n8)
DEF_SERIES (int32_t, 15, -1, 16, b0sm1n16)
DEF_SERIES (int32_t, 31, -1, 32, b0sm1n32)
DEF_SERIES (int32_t, 63, -1, 64, b0sm1n64)
DEF_SERIES (int32_t, 127, -1, 128, b0sm1n128)
DEF_SERIES (int32_t, 255, -1, 256, b0sm1n256)
DEF_SERIES (int32_t, 511, -1, 512, b0sm1n512)
DEF_SERIES (int32_t, 1023, -1, 1024, b0sm1n1024)

DEF_SERIES (int64_t, 1, -1, 2, b0sm1n2)
DEF_SERIES (int64_t, 3, -1, 4, b0sm1n4)
DEF_SERIES (int64_t, 7, -1, 8, b0sm1n8)
DEF_SERIES (int64_t, 15, -1, 16, b0sm1n16)
DEF_SERIES (int64_t, 31, -1, 32, b0sm1n32)
DEF_SERIES (int64_t, 63, -1, 64, b0sm1n64)
DEF_SERIES (int64_t, 127, -1, 128, b0sm1n128)
DEF_SERIES (int64_t, 255, -1, 256, b0sm1n256)
DEF_SERIES (int64_t, 511, -1, 512, b0sm1n512)

/* { dg-final { scan-assembler-times {vid\.v\s+v[0-9]+} 30 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
