/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V_CVT (lrintf, 1, float, long, __builtin_lrintf)
DEF_OP_V_CVT (lrintf, 2, float, long, __builtin_lrintf)
DEF_OP_V_CVT (lrintf, 4, float, long, __builtin_lrintf)
DEF_OP_V_CVT (lrintf, 8, float, long, __builtin_lrintf)
DEF_OP_V_CVT (lrintf, 16, float, long, __builtin_lrintf)
DEF_OP_V_CVT (lrintf, 32, float, long, __builtin_lrintf)
DEF_OP_V_CVT (lrintf, 64, float, long, __builtin_lrintf)
DEF_OP_V_CVT (lrintf, 128, float, long, __builtin_lrintf)
DEF_OP_V_CVT (lrintf, 256, float, long, __builtin_lrintf)
DEF_OP_V_CVT (lrintf, 512, float, long, __builtin_lrintf)

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
/* { dg-final { scan-assembler-times {vfwcvt\.x\.f\.v\s+v[0-9]+,\s*v[0-9]+} 9 } } */
