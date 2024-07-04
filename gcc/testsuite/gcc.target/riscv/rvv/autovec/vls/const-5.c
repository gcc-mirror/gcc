/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8 -fno-builtin" } */

#include "def.h"

DEF_CONST (int16_t, 116, 2)
DEF_CONST (int16_t, 116, 4)
DEF_CONST (int16_t, 116, 8)
DEF_CONST (int16_t, 116, 16)
DEF_CONST (int16_t, 116, 32)
DEF_CONST (int16_t, 116, 64)
DEF_CONST (int16_t, 116, 128)
DEF_CONST (int16_t, 116, 256)
DEF_CONST (int16_t, 116, 512)
DEF_CONST (int16_t, 116, 1024)
DEF_CONST (int16_t, 116, 2048)

DEF_CONST (int32_t, 116, 2)
DEF_CONST (int32_t, 116, 4)
DEF_CONST (int32_t, 116, 8)
DEF_CONST (int32_t, 116, 16)
DEF_CONST (int32_t, 116, 32)
DEF_CONST (int32_t, 116, 64)
DEF_CONST (int32_t, 116, 128)
DEF_CONST (int32_t, 116, 256)
DEF_CONST (int32_t, 116, 512)
DEF_CONST (int32_t, 116, 1024)

DEF_CONST (int64_t, 116, 2)
DEF_CONST (int64_t, 116, 4)
DEF_CONST (int64_t, 116, 8)
DEF_CONST (int64_t, 116, 16)
DEF_CONST (int64_t, 116, 32)
DEF_CONST (int64_t, 116, 64)
DEF_CONST (int64_t, 116, 128)
DEF_CONST (int64_t, 116, 256)
DEF_CONST (int64_t, 116, 512)

/* { dg-final { scan-assembler-times {vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 30 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
