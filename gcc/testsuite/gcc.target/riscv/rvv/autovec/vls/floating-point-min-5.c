/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8 -ffast-math" } */

#include "def.h"

DEF_CALL_VV (min, 1, float, __builtin_fminf)
DEF_CALL_VV (min, 2, float, __builtin_fminf)
DEF_CALL_VV (min, 4, float, __builtin_fminf)
DEF_CALL_VV (min, 8, float, __builtin_fminf)
DEF_CALL_VV (min, 16, float, __builtin_fminf)
DEF_CALL_VV (min, 32, float, __builtin_fminf)
DEF_CALL_VV (min, 64, float, __builtin_fminf)
DEF_CALL_VV (min, 128, float, __builtin_fminf)
DEF_CALL_VV (min, 256, float, __builtin_fminf)
DEF_CALL_VV (min, 512, float, __builtin_fminf)
DEF_CALL_VV (min, 1024, float, __builtin_fminf)

DEF_CALL_VV (min, 1, double, __builtin_fmin)
DEF_CALL_VV (min, 2, double, __builtin_fmin)
DEF_CALL_VV (min, 4, double, __builtin_fmin)
DEF_CALL_VV (min, 8, double, __builtin_fmin)
DEF_CALL_VV (min, 16, double, __builtin_fmin)
DEF_CALL_VV (min, 32, double, __builtin_fmin)
DEF_CALL_VV (min, 64, double, __builtin_fmin)
DEF_CALL_VV (min, 128, double, __builtin_fmin)
DEF_CALL_VV (min, 256, double, __builtin_fmin)
DEF_CALL_VV (min, 512, double, __builtin_fmin)

/* { dg-final { scan-assembler-times {vfmin\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 19 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
