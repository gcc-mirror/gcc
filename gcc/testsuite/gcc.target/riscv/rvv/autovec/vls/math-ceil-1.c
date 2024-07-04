/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V (ceilf16, 1, _Float16, __builtin_ceilf16)
DEF_OP_V (ceilf16, 2, _Float16, __builtin_ceilf16)
DEF_OP_V (ceilf16, 4, _Float16, __builtin_ceilf16)
DEF_OP_V (ceilf16, 8, _Float16, __builtin_ceilf16)
DEF_OP_V (ceilf16, 16, _Float16, __builtin_ceilf16)
DEF_OP_V (ceilf16, 32, _Float16, __builtin_ceilf16)
DEF_OP_V (ceilf16, 64, _Float16, __builtin_ceilf16)
DEF_OP_V (ceilf16, 128, _Float16, __builtin_ceilf16)
DEF_OP_V (ceilf16, 256, _Float16, __builtin_ceilf16)
DEF_OP_V (ceilf16, 512, _Float16, __builtin_ceilf16)
DEF_OP_V (ceilf16, 1024, _Float16, __builtin_ceilf16)
DEF_OP_V (ceilf16, 2048, _Float16, __builtin_ceilf16)

DEF_OP_V (ceilf, 1, float, __builtin_ceilf)
DEF_OP_V (ceilf, 2, float, __builtin_ceilf)
DEF_OP_V (ceilf, 4, float, __builtin_ceilf)
DEF_OP_V (ceilf, 8, float, __builtin_ceilf)
DEF_OP_V (ceilf, 16, float, __builtin_ceilf)
DEF_OP_V (ceilf, 32, float, __builtin_ceilf)
DEF_OP_V (ceilf, 64, float, __builtin_ceilf)
DEF_OP_V (ceilf, 128, float, __builtin_ceilf)
DEF_OP_V (ceilf, 256, float, __builtin_ceilf)
DEF_OP_V (ceilf, 512, float, __builtin_ceilf)
DEF_OP_V (ceilf, 1024, float, __builtin_ceilf)

DEF_OP_V (ceil, 1, double, __builtin_ceil)
DEF_OP_V (ceil, 2, double, __builtin_ceil)
DEF_OP_V (ceil, 4, double, __builtin_ceil)
DEF_OP_V (ceil, 8, double, __builtin_ceil)
DEF_OP_V (ceil, 16, double, __builtin_ceil)
DEF_OP_V (ceil, 32, double, __builtin_ceil)
DEF_OP_V (ceil, 64, double, __builtin_ceil)
DEF_OP_V (ceil, 128, double, __builtin_ceil)
DEF_OP_V (ceil, 256, double, __builtin_ceil)
DEF_OP_V (ceil, 512, double, __builtin_ceil)

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
/* { dg-final { scan-assembler-times {vfcvt\.x\.f\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0\.t} 30 } } */
/* { dg-final { scan-assembler-times {vfcvt\.f\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0\.t} 30 } } */
