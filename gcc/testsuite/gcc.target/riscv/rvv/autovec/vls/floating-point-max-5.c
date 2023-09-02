/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 --param=riscv-autovec-lmul=m8 -ffast-math" } */

#include "def.h"
#include "math.h"

DEF_CALL_VV (max, 1, float, fmaxf)
DEF_CALL_VV (max, 2, float, fmaxf)
DEF_CALL_VV (max, 4, float, fmaxf)
DEF_CALL_VV (max, 8, float, fmaxf)
DEF_CALL_VV (max, 16, float, fmaxf)
DEF_CALL_VV (max, 32, float, fmaxf)
DEF_CALL_VV (max, 64, float, fmaxf)
DEF_CALL_VV (max, 128, float, fmaxf)
DEF_CALL_VV (max, 256, float, fmaxf)
DEF_CALL_VV (max, 512, float, fmaxf)
DEF_CALL_VV (max, 1024, float, fmaxf)

DEF_CALL_VV (max, 1, double, fmax)
DEF_CALL_VV (max, 2, double, fmax)
DEF_CALL_VV (max, 4, double, fmax)
DEF_CALL_VV (max, 8, double, fmax)
DEF_CALL_VV (max, 16, double, fmax)
DEF_CALL_VV (max, 32, double, fmax)
DEF_CALL_VV (max, 64, double, fmax)
DEF_CALL_VV (max, 128, double, fmax)
DEF_CALL_VV (max, 256, double, fmax)
DEF_CALL_VV (max, 512, double, fmax)

/* { dg-final { scan-assembler-times {vfmax\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 19 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
