/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8 -fno-builtin" } */

#include "def.h"

DEF_CONST (int16_t, -16, 2)
DEF_CONST (int16_t, -16, 4)
DEF_CONST (int16_t, -16, 8)
DEF_CONST (int16_t, -16, 16)
DEF_CONST (int16_t, -16, 32)
DEF_CONST (int16_t, -16, 64)
DEF_CONST (int16_t, -16, 128)
DEF_CONST (int16_t, -16, 256)
DEF_CONST (int16_t, -16, 512)
DEF_CONST (int16_t, -16, 1024)
DEF_CONST (int16_t, -16, 2048)

DEF_CONST (int32_t, -16, 2)
DEF_CONST (int32_t, -16, 4)
DEF_CONST (int32_t, -16, 8)
DEF_CONST (int32_t, -16, 16)
DEF_CONST (int32_t, -16, 32)
DEF_CONST (int32_t, -16, 64)
DEF_CONST (int32_t, -16, 128)
DEF_CONST (int32_t, -16, 256)
DEF_CONST (int32_t, -16, 512)
DEF_CONST (int32_t, -16, 1024)

DEF_CONST (int64_t, -16, 2)
DEF_CONST (int64_t, -16, 4)
DEF_CONST (int64_t, -16, 8)
DEF_CONST (int64_t, -16, 16)
DEF_CONST (int64_t, -16, 32)
DEF_CONST (int64_t, -16, 64)
DEF_CONST (int64_t, -16, 128)
DEF_CONST (int64_t, -16, 256)
DEF_CONST (int64_t, -16, 512)

/* { dg-final { scan-assembler-times {vmv\.v\.i\s+v[0-9]+,\s*-16} 30 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
