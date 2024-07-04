/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8 -ffast-math" } */

#include "def.h"

DEF_MINMAX_VX (max, 1, _Float16, >=)
DEF_MINMAX_VX (max, 2, _Float16, >=)
DEF_MINMAX_VX (max, 4, _Float16, >=)
DEF_MINMAX_VX (max, 8, _Float16, >=)
DEF_MINMAX_VX (max, 16, _Float16, >=)
DEF_MINMAX_VX (max, 32, _Float16, >=)
DEF_MINMAX_VX (max, 64, _Float16, >=)
DEF_MINMAX_VX (max, 128, _Float16, >=)
DEF_MINMAX_VX (max, 256, _Float16, >=)
DEF_MINMAX_VX (max, 512, _Float16, >=)
DEF_MINMAX_VX (max, 1024, _Float16, >=)
DEF_MINMAX_VX (max, 2048, _Float16, >=)

DEF_MINMAX_VX (max, 1, float, >=)
DEF_MINMAX_VX (max, 2, float, >=)
DEF_MINMAX_VX (max, 4, float, >=)
DEF_MINMAX_VX (max, 8, float, >=)
DEF_MINMAX_VX (max, 16, float, >=)
DEF_MINMAX_VX (max, 32, float, >=)
DEF_MINMAX_VX (max, 64, float, >=)
DEF_MINMAX_VX (max, 128, float, >=)
DEF_MINMAX_VX (max, 256, float, >=)
DEF_MINMAX_VX (max, 512, float, >=)
DEF_MINMAX_VX (max, 1024, float, >=)

DEF_MINMAX_VX (max, 1, double, >=)
DEF_MINMAX_VX (max, 2, double, >=)
DEF_MINMAX_VX (max, 4, double, >=)
DEF_MINMAX_VX (max, 8, double, >=)
DEF_MINMAX_VX (max, 16, double, >=)
DEF_MINMAX_VX (max, 32, double, >=)
DEF_MINMAX_VX (max, 64, double, >=)
DEF_MINMAX_VX (max, 128, double, >=)
DEF_MINMAX_VX (max, 256, double, >=)
DEF_MINMAX_VX (max, 512, double, >=)

/* { dg-final { scan-assembler-times {vfmax\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 30 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
