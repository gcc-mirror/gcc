/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvfh_zvl4096b -mabi=ilp32d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V_CVT (lrint, 1, double, long, __builtin_lrint)
DEF_OP_V_CVT (lrint, 2, double, long, __builtin_lrint)
DEF_OP_V_CVT (lrint, 4, double, long, __builtin_lrint)
DEF_OP_V_CVT (lrint, 8, double, long, __builtin_lrint)
DEF_OP_V_CVT (lrint, 16, double, long, __builtin_lrint)
DEF_OP_V_CVT (lrint, 32, double, long, __builtin_lrint)
DEF_OP_V_CVT (lrint, 64, double, long, __builtin_lrint)
DEF_OP_V_CVT (lrint, 128, double, long, __builtin_lrint)
DEF_OP_V_CVT (lrint, 256, double, long, __builtin_lrint)
DEF_OP_V_CVT (lrint, 512, double, long, __builtin_lrint)

/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-tree-dump-not "1,1" "optimized" } } */
/* { dg-final { scan-tree-dump-not "2,2" "optimized" } } */
/* { dg-final { scan-tree-dump-not "4,4" "optimized" } } */
/* { dg-final { scan-tree-dump-not "16,16" "optimized" } } */
/* { dg-final { scan-tree-dump-not "32,32" "optimized" } } */
/* { dg-final { scan-tree-dump-not "64,64" "optimized" } } */
/* { dg-final { scan-tree-dump-not "128,128" "optimized" } } */
/* { dg-final { scan-tree-dump-not "256,256" "optimized" } } */
/* { dg-final { scan-tree-dump-not "512,512" "optimized" } } */
/* { dg-final { scan-tree-dump-not "1024,1024" "optimized" } } */
/* { dg-final { scan-tree-dump-not "2048,2048" "optimized" } } */
/* { dg-final { scan-tree-dump-not "4096,4096" "optimized" } } */
/* { dg-final { scan-assembler-times {vfncvt\.x\.f\.w\s+v[0-9]+,\s*v[0-9]+} 9 } } */
