/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V (sqrt, 2, _Float16, __builtin_sqrtf16)
DEF_OP_V (sqrt, 4, _Float16, __builtin_sqrtf16)
DEF_OP_V (sqrt, 8, _Float16, __builtin_sqrtf16)
DEF_OP_V (sqrt, 16, _Float16, __builtin_sqrtf16)
DEF_OP_V (sqrt, 32, _Float16, __builtin_sqrtf16)
DEF_OP_V (sqrt, 64, _Float16, __builtin_sqrtf16)
DEF_OP_V (sqrt, 128, _Float16, __builtin_sqrtf16)
DEF_OP_V (sqrt, 256, _Float16, __builtin_sqrtf16)
DEF_OP_V (sqrt, 512, _Float16, __builtin_sqrtf16)
DEF_OP_V (sqrt, 1024, _Float16, __builtin_sqrtf16)
DEF_OP_V (sqrt, 2048, _Float16, __builtin_sqrtf16)

DEF_OP_V (sqrt, 2, float, __builtin_sqrtf)
DEF_OP_V (sqrt, 4, float, __builtin_sqrtf)
DEF_OP_V (sqrt, 8, float, __builtin_sqrtf)
DEF_OP_V (sqrt, 16, float, __builtin_sqrtf)
DEF_OP_V (sqrt, 32, float, __builtin_sqrtf)
DEF_OP_V (sqrt, 64, float, __builtin_sqrtf)
DEF_OP_V (sqrt, 128, float, __builtin_sqrtf)
DEF_OP_V (sqrt, 256, float, __builtin_sqrtf)
DEF_OP_V (sqrt, 512, float, __builtin_sqrtf)
DEF_OP_V (sqrt, 1024, float, __builtin_sqrtf)

DEF_OP_V (sqrt, 2, double, __builtin_sqrt)
DEF_OP_V (sqrt, 4, double, __builtin_sqrt)
DEF_OP_V (sqrt, 8, double, __builtin_sqrt)
DEF_OP_V (sqrt, 16, double, __builtin_sqrt)
DEF_OP_V (sqrt, 32, double, __builtin_sqrt)
DEF_OP_V (sqrt, 64, double, __builtin_sqrt)
DEF_OP_V (sqrt, 128, double, __builtin_sqrt)
DEF_OP_V (sqrt, 256, double, __builtin_sqrt)
DEF_OP_V (sqrt, 512, double, __builtin_sqrt)

/* { dg-final { scan-assembler-times {vfsqrt\.v\s+v[0-9]+,\s*v[0-9]+} 30 } } */
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
