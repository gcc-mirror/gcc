/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvl4096b -mabi=ilp32f -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V_CVT (lfloorf, 1, float, long, __builtin_lfloorf)
DEF_OP_V_CVT (lfloorf, 2, float, long, __builtin_lfloorf)
DEF_OP_V_CVT (lfloorf, 4, float, long, __builtin_lfloorf)
DEF_OP_V_CVT (lfloorf, 8, float, long, __builtin_lfloorf)
DEF_OP_V_CVT (lfloorf, 16, float, long, __builtin_lfloorf)
DEF_OP_V_CVT (lfloorf, 32, float, long, __builtin_lfloorf)
DEF_OP_V_CVT (lfloorf, 64, float, long, __builtin_lfloorf)
DEF_OP_V_CVT (lfloorf, 128, float, long, __builtin_lfloorf)
DEF_OP_V_CVT (lfloorf, 256, float, long, __builtin_lfloorf)
DEF_OP_V_CVT (lfloorf, 512, float, long, __builtin_lfloorf)

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
/* { dg-final { scan-assembler-times {vfcvt\.x\.f\.v\s+v[0-9]+,\s*v[0-9]+} 9 } } */
