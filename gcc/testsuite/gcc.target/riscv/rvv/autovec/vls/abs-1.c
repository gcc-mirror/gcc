/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V (fabs, 2, _Float16, __builtin_fabs)
DEF_OP_V (fabs, 4, _Float16, __builtin_fabs)
DEF_OP_V (fabs, 8, _Float16, __builtin_fabs)
DEF_OP_V (fabs, 16, _Float16, __builtin_fabs)
DEF_OP_V (fabs, 32, _Float16, __builtin_fabs)
DEF_OP_V (fabs, 64, _Float16, __builtin_fabs)
DEF_OP_V (fabs, 128, _Float16, __builtin_fabs)
DEF_OP_V (fabs, 256, _Float16, __builtin_fabs)
DEF_OP_V (fabs, 512, _Float16, __builtin_fabs)
DEF_OP_V (fabs, 1024, _Float16, __builtin_fabs)
DEF_OP_V (fabs, 2048, _Float16, __builtin_fabs)

DEF_OP_V (fabs, 2, float, __builtin_fabs)
DEF_OP_V (fabs, 4, float, __builtin_fabs)
DEF_OP_V (fabs, 8, float, __builtin_fabs)
DEF_OP_V (fabs, 16, float, __builtin_fabs)
DEF_OP_V (fabs, 32, float, __builtin_fabs)
DEF_OP_V (fabs, 64, float, __builtin_fabs)
DEF_OP_V (fabs, 128, float, __builtin_fabs)
DEF_OP_V (fabs, 256, float, __builtin_fabs)
DEF_OP_V (fabs, 512, float, __builtin_fabs)
DEF_OP_V (fabs, 1024, float, __builtin_fabs)

DEF_OP_V (fabs, 2, double, __builtin_fabs)
DEF_OP_V (fabs, 4, double, __builtin_fabs)
DEF_OP_V (fabs, 8, double, __builtin_fabs)
DEF_OP_V (fabs, 16, double, __builtin_fabs)
DEF_OP_V (fabs, 32, double, __builtin_fabs)
DEF_OP_V (fabs, 64, double, __builtin_fabs)
DEF_OP_V (fabs, 128, double, __builtin_fabs)
DEF_OP_V (fabs, 256, double, __builtin_fabs)
DEF_OP_V (fabs, 512, double, __builtin_fabs)

/* { dg-final { scan-assembler-times {vfabs\.v\s+v[0-9]+,\s*v[0-9]+} 30 } } */
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
