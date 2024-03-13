/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8 -ffast-math" } */

#include "def.h"

DEF_SGNJX_VV (sgnj, 1, float, __builtin_copysignf)
DEF_SGNJX_VV (sgnj, 2, float, __builtin_copysignf)
DEF_SGNJX_VV (sgnj, 4, float, __builtin_copysignf)
DEF_SGNJX_VV (sgnj, 8, float, __builtin_copysignf)
DEF_SGNJX_VV (sgnj, 16, float, __builtin_copysignf)
DEF_SGNJX_VV (sgnj, 32, float, __builtin_copysignf)
DEF_SGNJX_VV (sgnj, 64, float, __builtin_copysignf)
DEF_SGNJX_VV (sgnj, 128, float, __builtin_copysignf)
DEF_SGNJX_VV (sgnj, 256, float, __builtin_copysignf)
DEF_SGNJX_VV (sgnj, 512, float, __builtin_copysignf)
DEF_SGNJX_VV (sgnj, 1024, float, __builtin_copysignf)

DEF_SGNJX_VV (sgnj, 1, double, __builtin_copysign)
DEF_SGNJX_VV (sgnj, 2, double, __builtin_copysign)
DEF_SGNJX_VV (sgnj, 4, double, __builtin_copysign)
DEF_SGNJX_VV (sgnj, 8, double, __builtin_copysign)
DEF_SGNJX_VV (sgnj, 16, double, __builtin_copysign)
DEF_SGNJX_VV (sgnj, 32, double, __builtin_copysign)
DEF_SGNJX_VV (sgnj, 64, double, __builtin_copysign)
DEF_SGNJX_VV (sgnj, 128, double, __builtin_copysign)
DEF_SGNJX_VV (sgnj, 256, double, __builtin_copysign)
DEF_SGNJX_VV (sgnj, 512, double, __builtin_copysign)

/* { dg-final { scan-assembler-times {vfsgnjx\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 19 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
