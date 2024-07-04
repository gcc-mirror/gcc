/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_COND_OP_WVV (cond_wadd, 4, hf, sf, float, +)
DEF_COND_OP_WVV (cond_wadd, 8, hf, sf, float, +)
DEF_COND_OP_WVV (cond_wadd, 16, hf, sf, float, +)
DEF_COND_OP_WVV (cond_wadd, 32, hf, sf, float, +)
DEF_COND_OP_WVV (cond_wadd, 64, hf, sf, float, +)
DEF_COND_OP_WVV (cond_wadd, 128, hf, sf, float, +)
DEF_COND_OP_WVV (cond_wadd, 256, hf, sf, float, +)
DEF_COND_OP_WVV (cond_wadd, 512, hf, sf, float, +)
DEF_COND_OP_WVV (cond_wadd, 1024, hf, sf, float, +)

DEF_COND_OP_WVV (cond_wadd, 4, sf, df, double, +)
DEF_COND_OP_WVV (cond_wadd, 8, sf, df, double, +)
DEF_COND_OP_WVV (cond_wadd, 16, sf, df, double, +)
DEF_COND_OP_WVV (cond_wadd, 32, sf, df, double, +)
DEF_COND_OP_WVV (cond_wadd, 64, sf, df, double, +)
DEF_COND_OP_WVV (cond_wadd, 128, sf, df, double, +)
DEF_COND_OP_WVV (cond_wadd, 256, sf, df, double, +)
DEF_COND_OP_WVV (cond_wadd, 512, sf, df, double, +)

/* { dg-final { scan-assembler-times {vfwadd\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 17 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-not {vmerge} } } */
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
