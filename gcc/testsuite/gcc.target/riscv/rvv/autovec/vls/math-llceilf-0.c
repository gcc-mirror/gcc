/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_OP_V_CVT (llceilf, 1, float, int64_t, __builtin_llceilf)
DEF_OP_V_CVT (llceilf, 2, float, int64_t, __builtin_llceilf)
DEF_OP_V_CVT (llceilf, 4, float, int64_t, __builtin_llceilf)
DEF_OP_V_CVT (llceilf, 8, float, int64_t, __builtin_llceilf)
DEF_OP_V_CVT (llceilf, 16, float, int64_t, __builtin_llceilf)
DEF_OP_V_CVT (llceilf, 32, float, int64_t, __builtin_llceilf)
DEF_OP_V_CVT (llceilf, 64, float, int64_t, __builtin_llceilf)
DEF_OP_V_CVT (llceilf, 128, float, int64_t, __builtin_llceilf)
DEF_OP_V_CVT (llceilf, 256, float, int64_t, __builtin_llceilf)
DEF_OP_V_CVT (llceilf, 512, float, int64_t, __builtin_llceilf)

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
/* { dg-final { scan-assembler-times {vfwcvt\.x\.f\.v\s+v[0-9]+,\s*v[0-9]+} 9 } } */
