/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_OP_VI_15 (add, 1, _Float16, +)
DEF_OP_VI_15 (add, 2, _Float16, +)
DEF_OP_VI_15 (add, 4, _Float16, +)
DEF_OP_VI_15 (add, 8, _Float16, +)
DEF_OP_VI_15 (add, 16, _Float16, +)
DEF_OP_VI_15 (add, 32, _Float16, +)
DEF_OP_VI_15 (add, 64, _Float16, +)
DEF_OP_VI_15 (add, 128, _Float16, +)
DEF_OP_VI_15 (add, 256, _Float16, +)
DEF_OP_VI_15 (add, 512, _Float16, +)
DEF_OP_VI_15 (add, 1024, _Float16, +)
DEF_OP_VI_15 (add, 2048, _Float16, +)

DEF_OP_VI_15 (add, 1, float, +)
DEF_OP_VI_15 (add, 2, float, +)
DEF_OP_VI_15 (add, 4, float, +)
DEF_OP_VI_15 (add, 8, float, +)
DEF_OP_VI_15 (add, 16, float, +)
DEF_OP_VI_15 (add, 32, float, +)
DEF_OP_VI_15 (add, 64, float, +)
DEF_OP_VI_15 (add, 128, float, +)
DEF_OP_VI_15 (add, 256, float, +)
DEF_OP_VI_15 (add, 512, float, +)
DEF_OP_VI_15 (add, 1024, float, +)

DEF_OP_VI_15 (add, 1, double, +)
DEF_OP_VI_15 (add, 2, double, +)
DEF_OP_VI_15 (add, 4, double, +)
DEF_OP_VI_15 (add, 8, double, +)
DEF_OP_VI_15 (add, 16, double, +)
DEF_OP_VI_15 (add, 32, double, +)
DEF_OP_VI_15 (add, 64, double, +)
DEF_OP_VI_15 (add, 128, double, +)
DEF_OP_VI_15 (add, 256, double, +)
DEF_OP_VI_15 (add, 512, double, +)

/* { dg-final { scan-assembler-times {vfadd\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 30 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
