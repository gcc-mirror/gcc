/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8 -fno-builtin" } */

#include "def.h"

DEF_SERIES (int16_t, 0, 1, 2, b0s1n2)
DEF_SERIES (int16_t, 0, 1, 4, b0s1n4)
DEF_SERIES (int16_t, 0, 1, 8, b0s1n8)
DEF_SERIES (int16_t, 0, 1, 16, b0s1n16)
DEF_SERIES (int16_t, 0, 1, 32, b0s1n32)
DEF_SERIES (int16_t, 0, 1, 64, b0s1n64)
DEF_SERIES (int16_t, 0, 1, 128, b0s1n128)
DEF_SERIES (int16_t, 0, 1, 256, b0s1n256)
DEF_SERIES (int16_t, 0, 1, 512, b0s1n512)
DEF_SERIES (int16_t, 0, 1, 1024, b0s1n1024)
DEF_SERIES (int16_t, 0, 1, 2048, b0s1n2048)

DEF_SERIES (int32_t, 0, 1, 2, b0s1n2)
DEF_SERIES (int32_t, 0, 1, 4, b0s1n4)
DEF_SERIES (int32_t, 0, 1, 8, b0s1n8)
DEF_SERIES (int32_t, 0, 1, 16, b0s1n16)
DEF_SERIES (int32_t, 0, 1, 32, b0s1n32)
DEF_SERIES (int32_t, 0, 1, 64, b0s1n64)
DEF_SERIES (int32_t, 0, 1, 128, b0s1n128)
DEF_SERIES (int32_t, 0, 1, 256, b0s1n256)
DEF_SERIES (int32_t, 0, 1, 512, b0s1n512)
DEF_SERIES (int32_t, 0, 1, 1024, b0s1n1024)

DEF_SERIES (int64_t, 0, 1, 2, b0s1n2)
DEF_SERIES (int64_t, 0, 1, 4, b0s1n4)
DEF_SERIES (int64_t, 0, 1, 8, b0s1n8)
DEF_SERIES (int64_t, 0, 1, 16, b0s1n16)
DEF_SERIES (int64_t, 0, 1, 32, b0s1n32)
DEF_SERIES (int64_t, 0, 1, 64, b0s1n64)
DEF_SERIES (int64_t, 0, 1, 128, b0s1n128)
DEF_SERIES (int64_t, 0, 1, 256, b0s1n256)
DEF_SERIES (int64_t, 0, 1, 512, b0s1n512)

/* { dg-final { scan-assembler-times {vid\.v\s+v[0-9]+} 30 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
