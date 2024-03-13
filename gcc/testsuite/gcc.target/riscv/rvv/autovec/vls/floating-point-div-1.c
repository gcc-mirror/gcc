/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_OP_VV (div, 1, _Float16, /)
DEF_OP_VV (div, 2, _Float16, /)
DEF_OP_VV (div, 4, _Float16, /)
DEF_OP_VV (div, 8, _Float16, /)
DEF_OP_VV (div, 16, _Float16, /)
DEF_OP_VV (div, 32, _Float16, /)
DEF_OP_VV (div, 64, _Float16, /)
DEF_OP_VV (div, 128, _Float16, /)
DEF_OP_VV (div, 256, _Float16, /)
DEF_OP_VV (div, 512, _Float16, /)
DEF_OP_VV (div, 1024, _Float16, /)
DEF_OP_VV (div, 2048, _Float16, /)

DEF_OP_VV (div, 1, float, /)
DEF_OP_VV (div, 2, float, /)
DEF_OP_VV (div, 4, float, /)
DEF_OP_VV (div, 8, float, /)
DEF_OP_VV (div, 16, float, /)
DEF_OP_VV (div, 32, float, /)
DEF_OP_VV (div, 64, float, /)
DEF_OP_VV (div, 128, float, /)
DEF_OP_VV (div, 256, float, /)
DEF_OP_VV (div, 512, float, /)
DEF_OP_VV (div, 1024, float, /)

DEF_OP_VV (div, 1, double, /)
DEF_OP_VV (div, 2, double, /)
DEF_OP_VV (div, 4, double, /)
DEF_OP_VV (div, 8, double, /)
DEF_OP_VV (div, 16, double, /)
DEF_OP_VV (div, 32, double, /)
DEF_OP_VV (div, 64, double, /)
DEF_OP_VV (div, 128, double, /)
DEF_OP_VV (div, 256, double, /)
DEF_OP_VV (div, 512, double, /)

/* { dg-final { scan-assembler-times {vfdiv\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 30 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
