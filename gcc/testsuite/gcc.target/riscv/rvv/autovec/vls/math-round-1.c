/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V (roundf16, 1, _Float16, __builtin_roundf16)
DEF_OP_V (roundf16, 2, _Float16, __builtin_roundf16)
DEF_OP_V (roundf16, 4, _Float16, __builtin_roundf16)
DEF_OP_V (roundf16, 8, _Float16, __builtin_roundf16)
DEF_OP_V (roundf16, 16, _Float16, __builtin_roundf16)
DEF_OP_V (roundf16, 32, _Float16, __builtin_roundf16)
DEF_OP_V (roundf16, 64, _Float16, __builtin_roundf16)
DEF_OP_V (roundf16, 128, _Float16, __builtin_roundf16)
DEF_OP_V (roundf16, 256, _Float16, __builtin_roundf16)
DEF_OP_V (roundf16, 512, _Float16, __builtin_roundf16)
DEF_OP_V (roundf16, 1024, _Float16, __builtin_roundf16)
DEF_OP_V (roundf16, 2048, _Float16, __builtin_roundf16)

DEF_OP_V (roundf, 1, float, __builtin_roundf)
DEF_OP_V (roundf, 2, float, __builtin_roundf)
DEF_OP_V (roundf, 4, float, __builtin_roundf)
DEF_OP_V (roundf, 8, float, __builtin_roundf)
DEF_OP_V (roundf, 16, float, __builtin_roundf)
DEF_OP_V (roundf, 32, float, __builtin_roundf)
DEF_OP_V (roundf, 64, float, __builtin_roundf)
DEF_OP_V (roundf, 128, float, __builtin_roundf)
DEF_OP_V (roundf, 256, float, __builtin_roundf)
DEF_OP_V (roundf, 512, float, __builtin_roundf)
DEF_OP_V (roundf, 1024, float, __builtin_roundf)

DEF_OP_V (round, 1, double, __builtin_round)
DEF_OP_V (round, 2, double, __builtin_round)
DEF_OP_V (round, 4, double, __builtin_round)
DEF_OP_V (round, 8, double, __builtin_round)
DEF_OP_V (round, 16, double, __builtin_round)
DEF_OP_V (round, 32, double, __builtin_round)
DEF_OP_V (round, 64, double, __builtin_round)
DEF_OP_V (round, 128, double, __builtin_round)
DEF_OP_V (round, 256, double, __builtin_round)
DEF_OP_V (round, 512, double, __builtin_round)

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
