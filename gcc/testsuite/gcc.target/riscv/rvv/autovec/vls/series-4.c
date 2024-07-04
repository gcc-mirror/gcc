/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8 -fno-builtin" } */

#include "def.h"

DEF_SERIES (int16_t, 67, 7, 2, b99s7n2)
DEF_SERIES (int16_t, 67, 7, 4, b99s7n4)
DEF_SERIES (int16_t, 67, 7, 8, b99s7n8)
DEF_SERIES (int16_t, 67, 7, 16, b99s7n16)
DEF_SERIES (int16_t, 67, 7, 32, b99s7n32)
DEF_SERIES (int16_t, 67, 7, 64, b99s7n64)
DEF_SERIES (int16_t, 67, 7, 128, b99s7n128)
DEF_SERIES (int16_t, 67, 7, 256, b99s7n256)
DEF_SERIES (int16_t, 67, 7, 512, b99s7n512)
DEF_SERIES (int16_t, 67, 7, 1024, b99s7n1024)
DEF_SERIES (int16_t, 67, 7, 2048, b99s7n2048)

DEF_SERIES (int32_t, 76, 7, 2, b99s7n2)
DEF_SERIES (int32_t, 76, 7, 4, b99s7n4)
DEF_SERIES (int32_t, 76, 7, 8, b99s7n8)
DEF_SERIES (int32_t, 76, 7, 16, b99s7n16)
DEF_SERIES (int32_t, 76, 7, 32, b99s7n32)
DEF_SERIES (int32_t, 76, 7, 64, b99s7n64)
DEF_SERIES (int32_t, 76, 7, 128, b99s7n128)
DEF_SERIES (int32_t, 76, 7, 256, b99s7n256)
DEF_SERIES (int32_t, 76, 7, 512, b99s7n512)
DEF_SERIES (int32_t, 76, 7, 1024, b99s7n1024)

DEF_SERIES (int64_t, 99, 7, 2, b99s7n2)
DEF_SERIES (int64_t, 99, 7, 4, b99s7n4)
DEF_SERIES (int64_t, 99, 7, 8, b99s7n8)
DEF_SERIES (int64_t, 99, 7, 16, b99s7n16)
DEF_SERIES (int64_t, 99, 7, 32, b99s7n32)
DEF_SERIES (int64_t, 99, 7, 64, b99s7n64)
DEF_SERIES (int64_t, 99, 7, 128, b99s7n128)
DEF_SERIES (int64_t, 99, 7, 256, b99s7n256)
DEF_SERIES (int64_t, 99, 7, 512, b99s7n512)

/* { dg-final { scan-assembler-times {vid\.v\s+v[0-9]+} 30 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
