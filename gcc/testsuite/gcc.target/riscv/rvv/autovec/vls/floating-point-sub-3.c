/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_OP_VI_15 (sub, 1, _Float16, -)
DEF_OP_VI_15 (sub, 2, _Float16, -)
DEF_OP_VI_15 (sub, 4, _Float16, -)
DEF_OP_VI_15 (sub, 8, _Float16, -)
DEF_OP_VI_15 (sub, 16, _Float16, -)
DEF_OP_VI_15 (sub, 32, _Float16, -)
DEF_OP_VI_15 (sub, 64, _Float16, -)
DEF_OP_VI_15 (sub, 128, _Float16, -)
DEF_OP_VI_15 (sub, 256, _Float16, -)
DEF_OP_VI_15 (sub, 512, _Float16, -)
DEF_OP_VI_15 (sub, 1024, _Float16, -)
DEF_OP_VI_15 (sub, 2048, _Float16, -)

DEF_OP_VI_15 (sub, 1, float, -)
DEF_OP_VI_15 (sub, 2, float, -)
DEF_OP_VI_15 (sub, 4, float, -)
DEF_OP_VI_15 (sub, 8, float, -)
DEF_OP_VI_15 (sub, 16, float, -)
DEF_OP_VI_15 (sub, 32, float, -)
DEF_OP_VI_15 (sub, 64, float, -)
DEF_OP_VI_15 (sub, 128, float, -)
DEF_OP_VI_15 (sub, 256, float, -)
DEF_OP_VI_15 (sub, 512, float, -)
DEF_OP_VI_15 (sub, 1024, float, -)

DEF_OP_VI_15 (sub, 1, double, -)
DEF_OP_VI_15 (sub, 2, double, -)
DEF_OP_VI_15 (sub, 4, double, -)
DEF_OP_VI_15 (sub, 8, double, -)
DEF_OP_VI_15 (sub, 16, double, -)
DEF_OP_VI_15 (sub, 32, double, -)
DEF_OP_VI_15 (sub, 64, double, -)
DEF_OP_VI_15 (sub, 128, double, -)
DEF_OP_VI_15 (sub, 256, double, -)
DEF_OP_VI_15 (sub, 512, double, -)

/* { dg-final { scan-assembler-times {vfadd\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 30 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
