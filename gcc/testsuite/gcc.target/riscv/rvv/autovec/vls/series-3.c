/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8 -fno-builtin" } */

#include "def.h"

DEF_SERIES (int16_t, 0, 8, 2, b0s8n2)
DEF_SERIES (int16_t, 0, 8, 4, b0s8n4)
DEF_SERIES (int16_t, 0, 8, 8, b0s8n8)
DEF_SERIES (int16_t, 0, 8, 16, b0s8n16)
DEF_SERIES (int16_t, 0, 8, 32, b0s8n32)
DEF_SERIES (int16_t, 0, 8, 64, b0s8n64)
DEF_SERIES (int16_t, 0, 8, 128, b0s8n128)
DEF_SERIES (int16_t, 0, 8, 256, b0s8n256)
DEF_SERIES (int16_t, 0, 8, 512, b0s8n512)
DEF_SERIES (int16_t, 0, 8, 1024, b0s8n1024)
DEF_SERIES (int16_t, 0, 8, 2048, b0s8n2048)

DEF_SERIES (int32_t, 0, 8, 2, b0s8n2)
DEF_SERIES (int32_t, 0, 8, 4, b0s8n4)
DEF_SERIES (int32_t, 0, 8, 8, b0s8n8)
DEF_SERIES (int32_t, 0, 8, 16, b0s8n16)
DEF_SERIES (int32_t, 0, 8, 32, b0s8n32)
DEF_SERIES (int32_t, 0, 8, 64, b0s8n64)
DEF_SERIES (int32_t, 0, 8, 128, b0s8n128)
DEF_SERIES (int32_t, 0, 8, 256, b0s8n256)
DEF_SERIES (int32_t, 0, 8, 512, b0s8n512)
DEF_SERIES (int32_t, 0, 8, 1024, b0s8n1024)

DEF_SERIES (int64_t, 0, 8, 2, b0s8n2)
DEF_SERIES (int64_t, 0, 8, 4, b0s8n4)
DEF_SERIES (int64_t, 0, 8, 8, b0s8n8)
DEF_SERIES (int64_t, 0, 8, 16, b0s8n16)
DEF_SERIES (int64_t, 0, 8, 32, b0s8n32)
DEF_SERIES (int64_t, 0, 8, 64, b0s8n64)
DEF_SERIES (int64_t, 0, 8, 128, b0s8n128)
DEF_SERIES (int64_t, 0, 8, 256, b0s8n256)
DEF_SERIES (int64_t, 0, 8, 512, b0s8n512)

/* { dg-final { scan-assembler-times {vid\.v\s+v[0-9]+} 30 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
