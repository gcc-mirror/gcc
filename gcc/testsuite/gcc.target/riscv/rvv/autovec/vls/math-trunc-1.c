/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V (truncf16, 1, _Float16, __builtin_truncf16)
DEF_OP_V (truncf16, 2, _Float16, __builtin_truncf16)
DEF_OP_V (truncf16, 4, _Float16, __builtin_truncf16)
DEF_OP_V (truncf16, 8, _Float16, __builtin_truncf16)
DEF_OP_V (truncf16, 16, _Float16, __builtin_truncf16)
DEF_OP_V (truncf16, 32, _Float16, __builtin_truncf16)
DEF_OP_V (truncf16, 64, _Float16, __builtin_truncf16)
DEF_OP_V (truncf16, 128, _Float16, __builtin_truncf16)
DEF_OP_V (truncf16, 256, _Float16, __builtin_truncf16)
DEF_OP_V (truncf16, 512, _Float16, __builtin_truncf16)
DEF_OP_V (truncf16, 1024, _Float16, __builtin_truncf16)
DEF_OP_V (truncf16, 2048, _Float16, __builtin_truncf16)

DEF_OP_V (truncf, 1, float, __builtin_truncf)
DEF_OP_V (truncf, 2, float, __builtin_truncf)
DEF_OP_V (truncf, 4, float, __builtin_truncf)
DEF_OP_V (truncf, 8, float, __builtin_truncf)
DEF_OP_V (truncf, 16, float, __builtin_truncf)
DEF_OP_V (truncf, 32, float, __builtin_truncf)
DEF_OP_V (truncf, 64, float, __builtin_truncf)
DEF_OP_V (truncf, 128, float, __builtin_truncf)
DEF_OP_V (truncf, 256, float, __builtin_truncf)
DEF_OP_V (truncf, 512, float, __builtin_truncf)
DEF_OP_V (truncf, 1024, float, __builtin_truncf)

DEF_OP_V (trunc, 1, double, __builtin_trunc)
DEF_OP_V (trunc, 2, double, __builtin_trunc)
DEF_OP_V (trunc, 4, double, __builtin_trunc)
DEF_OP_V (trunc, 8, double, __builtin_trunc)
DEF_OP_V (trunc, 16, double, __builtin_trunc)
DEF_OP_V (trunc, 32, double, __builtin_trunc)
DEF_OP_V (trunc, 64, double, __builtin_trunc)
DEF_OP_V (trunc, 128, double, __builtin_trunc)
DEF_OP_V (trunc, 256, double, __builtin_trunc)
DEF_OP_V (trunc, 512, double, __builtin_trunc)

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
/* { dg-final { scan-assembler-times {vfcvt\.rtz\.x\.f\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0\.t} 30 } } */
/* { dg-final { scan-assembler-times {vfcvt\.f\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0\.t} 30 } } */
