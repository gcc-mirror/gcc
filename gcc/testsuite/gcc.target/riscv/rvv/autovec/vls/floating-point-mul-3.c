/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_OP_VI_15 (mul, 1, _Float16, *)
DEF_OP_VI_15 (mul, 2, _Float16, *)
DEF_OP_VI_15 (mul, 4, _Float16, *)
DEF_OP_VI_15 (mul, 8, _Float16, *)
DEF_OP_VI_15 (mul, 16, _Float16, *)
DEF_OP_VI_15 (mul, 32, _Float16, *)
DEF_OP_VI_15 (mul, 64, _Float16, *)
DEF_OP_VI_15 (mul, 128, _Float16, *)
DEF_OP_VI_15 (mul, 256, _Float16, *)
DEF_OP_VI_15 (mul, 512, _Float16, *)
DEF_OP_VI_15 (mul, 1024, _Float16, *)
DEF_OP_VI_15 (mul, 2048, _Float16, *)

DEF_OP_VI_15 (mul, 1, float, *)
DEF_OP_VI_15 (mul, 2, float, *)
DEF_OP_VI_15 (mul, 4, float, *)
DEF_OP_VI_15 (mul, 8, float, *)
DEF_OP_VI_15 (mul, 16, float, *)
DEF_OP_VI_15 (mul, 32, float, *)
DEF_OP_VI_15 (mul, 64, float, *)
DEF_OP_VI_15 (mul, 128, float, *)
DEF_OP_VI_15 (mul, 256, float, *)
DEF_OP_VI_15 (mul, 512, float, *)
DEF_OP_VI_15 (mul, 1024, float, *)

DEF_OP_VI_15 (mul, 1, double, *)
DEF_OP_VI_15 (mul, 2, double, *)
DEF_OP_VI_15 (mul, 4, double, *)
DEF_OP_VI_15 (mul, 8, double, *)
DEF_OP_VI_15 (mul, 16, double, *)
DEF_OP_VI_15 (mul, 32, double, *)
DEF_OP_VI_15 (mul, 64, double, *)
DEF_OP_VI_15 (mul, 128, double, *)
DEF_OP_VI_15 (mul, 256, double, *)
DEF_OP_VI_15 (mul, 512, double, *)

/* { dg-final { scan-assembler-times {vfmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 30 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
