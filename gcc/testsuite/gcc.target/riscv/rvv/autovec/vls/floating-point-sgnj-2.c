/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8 -ffast-math" } */

#include "def.h"

DEF_CALL_VX (sgnj, 1, _Float16, __builtin_copysignf16)
DEF_CALL_VX (sgnj, 2, _Float16, __builtin_copysignf16)
DEF_CALL_VX (sgnj, 4, _Float16, __builtin_copysignf16)
DEF_CALL_VX (sgnj, 8, _Float16, __builtin_copysignf16)
DEF_CALL_VX (sgnj, 16, _Float16, __builtin_copysignf16)
DEF_CALL_VX (sgnj, 32, _Float16, __builtin_copysignf16)
DEF_CALL_VX (sgnj, 64, _Float16, __builtin_copysignf16)
DEF_CALL_VX (sgnj, 128, _Float16, __builtin_copysignf16)
DEF_CALL_VX (sgnj, 256, _Float16, __builtin_copysignf16)
DEF_CALL_VX (sgnj, 512, _Float16, __builtin_copysignf16)
DEF_CALL_VX (sgnj, 1024, _Float16, __builtin_copysignf16)
DEF_CALL_VX (sgnj, 2048, _Float16, __builtin_copysignf16)

DEF_CALL_VX (sgnj, 1, float, __builtin_copysignf)
DEF_CALL_VX (sgnj, 2, float, __builtin_copysignf)
DEF_CALL_VX (sgnj, 4, float, __builtin_copysignf)
DEF_CALL_VX (sgnj, 8, float, __builtin_copysignf)
DEF_CALL_VX (sgnj, 16, float, __builtin_copysignf)
DEF_CALL_VX (sgnj, 32, float, __builtin_copysignf)
DEF_CALL_VX (sgnj, 64, float, __builtin_copysignf)
DEF_CALL_VX (sgnj, 128, float, __builtin_copysignf)
DEF_CALL_VX (sgnj, 256, float, __builtin_copysignf)
DEF_CALL_VX (sgnj, 512, float, __builtin_copysignf)
DEF_CALL_VX (sgnj, 1024, float, __builtin_copysignf)

DEF_CALL_VX (sgnj, 1, double, __builtin_copysign)
DEF_CALL_VX (sgnj, 2, double, __builtin_copysign)
DEF_CALL_VX (sgnj, 4, double, __builtin_copysign)
DEF_CALL_VX (sgnj, 8, double, __builtin_copysign)
DEF_CALL_VX (sgnj, 16, double, __builtin_copysign)
DEF_CALL_VX (sgnj, 32, double, __builtin_copysign)
DEF_CALL_VX (sgnj, 64, double, __builtin_copysign)
DEF_CALL_VX (sgnj, 128, double, __builtin_copysign)
DEF_CALL_VX (sgnj, 256, double, __builtin_copysign)
DEF_CALL_VX (sgnj, 512, double, __builtin_copysign)

/* { dg-final { scan-assembler-times {vfsgnj\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 30 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
