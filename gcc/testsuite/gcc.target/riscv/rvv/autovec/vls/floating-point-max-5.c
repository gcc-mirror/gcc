/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8 -ffast-math" } */

#include "def.h"

DEF_CALL_VV (max, 1, float, __builtin_fmaxf)
DEF_CALL_VV (max, 2, float, __builtin_fmaxf)
DEF_CALL_VV (max, 4, float, __builtin_fmaxf)
DEF_CALL_VV (max, 8, float, __builtin_fmaxf)
DEF_CALL_VV (max, 16, float, __builtin_fmaxf)
DEF_CALL_VV (max, 32, float, __builtin_fmaxf)
DEF_CALL_VV (max, 64, float, __builtin_fmaxf)
DEF_CALL_VV (max, 128, float, __builtin_fmaxf)
DEF_CALL_VV (max, 256, float, __builtin_fmaxf)
DEF_CALL_VV (max, 512, float, __builtin_fmaxf)
DEF_CALL_VV (max, 1024, float, __builtin_fmaxf)

DEF_CALL_VV (max, 1, double, __builtin_fmax)
DEF_CALL_VV (max, 2, double, __builtin_fmax)
DEF_CALL_VV (max, 4, double, __builtin_fmax)
DEF_CALL_VV (max, 8, double, __builtin_fmax)
DEF_CALL_VV (max, 16, double, __builtin_fmax)
DEF_CALL_VV (max, 32, double, __builtin_fmax)
DEF_CALL_VV (max, 64, double, __builtin_fmax)
DEF_CALL_VV (max, 128, double, __builtin_fmax)
DEF_CALL_VV (max, 256, double, __builtin_fmax)
DEF_CALL_VV (max, 512, double, __builtin_fmax)

/* { dg-final { scan-assembler-times {vfmax\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 19 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
