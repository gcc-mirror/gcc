/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -ffast-math -fdump-tree-optimized" } */

#include "def.h"

DEF_COND_CONVERT (fwcvt, v4hf, v4sf, 4)
DEF_COND_CONVERT (fwcvt, v16hf, v16sf, 16)
DEF_COND_CONVERT (fwcvt, v32hf, v32sf, 32)
DEF_COND_CONVERT (fwcvt, v64hf, v64sf, 64)
DEF_COND_CONVERT (fwcvt, v128hf, v128sf, 128)
DEF_COND_CONVERT (fwcvt, v256hf, v256sf, 256)
DEF_COND_CONVERT (fwcvt, v512hf, v512sf, 512)
DEF_COND_CONVERT (fwcvt, v1024hf, v1024sf, 1024)

DEF_COND_CONVERT (fwcvt, v4sf, v4df, 4)
DEF_COND_CONVERT (fwcvt, v16sf, v16df, 16)
DEF_COND_CONVERT (fwcvt, v32sf, v32df, 32)
DEF_COND_CONVERT (fwcvt, v64sf, v64df, 64)
DEF_COND_CONVERT (fwcvt, v128sf, v128df, 128)
DEF_COND_CONVERT (fwcvt, v256sf, v256df, 256)
DEF_COND_CONVERT (fwcvt, v512sf, v512df, 512)

/* { dg-final { scan-assembler-times {vfwcvt\.f\.f\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 15 } } */
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
