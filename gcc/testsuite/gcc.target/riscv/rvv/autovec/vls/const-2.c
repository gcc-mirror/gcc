/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8 -fno-builtin" } */

#include "def.h"

DEF_CONST (int16_t, 15, 2)
DEF_CONST (int16_t, 15, 4)
DEF_CONST (int16_t, 15, 8)
DEF_CONST (int16_t, 15, 16)
DEF_CONST (int16_t, 15, 32)
DEF_CONST (int16_t, 15, 64)
DEF_CONST (int16_t, 15, 128)
DEF_CONST (int16_t, 15, 256)
DEF_CONST (int16_t, 15, 512)
DEF_CONST (int16_t, 15, 1024)
DEF_CONST (int16_t, 15, 2048)

DEF_CONST (int32_t, 15, 2)
DEF_CONST (int32_t, 15, 4)
DEF_CONST (int32_t, 15, 8)
DEF_CONST (int32_t, 15, 16)
DEF_CONST (int32_t, 15, 32)
DEF_CONST (int32_t, 15, 64)
DEF_CONST (int32_t, 15, 128)
DEF_CONST (int32_t, 15, 256)
DEF_CONST (int32_t, 15, 512)
DEF_CONST (int32_t, 15, 1024)

DEF_CONST (int64_t, 15, 2)
DEF_CONST (int64_t, 15, 4)
DEF_CONST (int64_t, 15, 8)
DEF_CONST (int64_t, 15, 16)
DEF_CONST (int64_t, 15, 32)
DEF_CONST (int64_t, 15, 64)
DEF_CONST (int64_t, 15, 128)
DEF_CONST (int64_t, 15, 256)
DEF_CONST (int64_t, 15, 512)

/* { dg-final { scan-assembler-times {vmv\.v\.i\s+v[0-9]+,\s*15} 30 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
